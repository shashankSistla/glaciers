main.function_07_fda <- function(key, root_dir){
# STEP_NAME
    step_name = "07_fda"

    # LIBRARIES
    library(splines)
    library(mgcv)
    library(scales)
    library(fields)
    library(grDevices)
    library(dplyr)
    library(zoo) # For rollapply
    library(fda)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/07_fda/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/07_fda/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/src/PCODE.R"))
    source(paste0(root_dir, "/keys/", key,".R"))

    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    step_04_output_dir = paste0(root_dir, "/output/", key, "/04_candidate_paths/output/")
    step_05_output_dir = paste0(root_dir, "/output/", key, "/05_clustering/output/")

    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir_path = paste0(output_dir, "/output")

    for(glacier in glacier_list){
        candidate_paths = readRDS(paste0(step_04_output_dir, glacier,"_candidate_paths.rds"))
        tt = readRDS(paste0(step_03_output_dir,glacier, "_dates_cut.rds"))
        al = readRDS(paste0(step_03_output_dir,glacier, "_al.rds"))
        paths <- lapply(candidate_paths, function(indices) {
            al[indices]
        })
        csv_filename = paste0(root_dir,"/data/GEE/",glacier,"_env.csv")


        L = paths[[1]]
        overall_mean_L <- mean(L, na.rm = TRUE)
        L_centered <- L - overall_mean_L
        
        env_data <- read.csv(csv_filename)
        env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")

        overall_mean_precipitation <- mean(env_data$Total_Precipitation, na.rm = TRUE)

        temp_above_273 <- env_data %>%
        filter(Temperature_2m > 273) %>%
        dplyr::select(Year, Month, Temperature_2m)


        overall_mean_summer_temperature <- temp_above_273 %>%
            summarise(Mean = mean(Temperature_2m, na.rm = TRUE)) %>%
        pull(Mean)

        env_data <- env_data %>%
        mutate(
            IsHighTempMonth = paste(Year, Month) %in% paste(temp_above_273$Year, temp_above_273$Month)
        )

        env_data <- env_data %>%
        mutate(
            PrecipitationDeparture = Total_Precipitation - overall_mean_precipitation,
            SummerTemperatureDeparture = ifelse(IsHighTempMonth, Temperature_2m - overall_mean_summer_temperature, NA)
        )

        env_data_rolling <- env_data %>%
        mutate(
            SummerTempDepartureRolling5Yr = rollapply(SummerTemperatureDeparture, 5, mean, partial = TRUE, align = "right"),
            PrecipDepartureRolling5Yr = rollapply(PrecipitationDeparture, 5, mean, partial = TRUE, align = "right")
        )
  
        summer_data_rolling_adjusted <- summer_data_adjusted %>%
        mutate(
            SummerTempDepartureRolling5Yr = rollapply(SummerAvgTempDeparture, 5, mean, partial = TRUE, align = "right")
        )

        interpolate_summer_temp_rolling <- function(input_year) {
            return(approx(x = summer_data_rolling_adjusted$Year, 
                                    y = summer_data_rolling_adjusted$SummerTempDepartureRolling5Yr, 
                                    xout = input_year, 
                                    rule = 2)$y)
        }
  
        interpolate_precip_rolling <- function(input_year) {
            return(approx(x = annual_data_rolling_adjusted$Year, 
                                        y = annual_data_rolling_adjusted$PrecipDepartureRolling5Yr, 
                                        xout = input_year, 
                                        rule = 2)$y)
        }

          ode_system <- function(t, state, parameters) {
            with(as.list(c(state, parameters)), {
            T_val = interpolate_summer_temp_rolling(t)
            P_val = interpolate_precip_rolling(t)
            dL_dt <- alpha * T_val + beta * P_val + gamma - X / tau_param
            return(list(dL_dt))
            })
        }

          par.names = c('alpha', 'beta', 'gamma', 'tau_param')
  
        # Spline basis for L, T, P
        number_of_knots = 10
        times = tt
        breaks <- quantile(times, probs = seq(0, 1, length.out = number_of_knots + 2)) # +2 for the boundary knots
        norder <- 4
        nbasis <- norder + length(breaks) - 2
        
        b_spline_basis_L <- create.bspline.basis(range = range(times), nbasis = nbasis, norder = norder, breaks = breaks)
        X_t = eval.basis(tt,b_spline_basis_L)
        print("Hi there")
        print(X_t)
        print(dim(X_t))
        print(typeof(X_t))
        print(class(X_t))
        
        spline_fit = mgcv::gam(L_centered ~ -1 + X_t, fx = TRUE)
        print("passed this")
        fitted_values <- predict(spline_fit, newdata = data.frame(X_t))
        print("did i pass this?")
        plot(tt, L_centered, type = 'l', col = 'blue', xlab = 'Time', ylab = 'L Centered', main = 'Original L Centered vs. Spline Fit')
        lines(tt, fitted_values, col = 'red')
        legend("topright", legend=c("Original L Centered", "Spline Fit"), col=c("blue", "red"), lty=1)

        par.initial <- c(alpha = 3.2, beta = -1, gamma = -4, tau_param = 20)
        state.names <- c('L_val')
        L_centered = as.numeric(L_centered
                                )
        result <- pcode(data = L_centered, time = times, ode.model = ode_system,
                        par.names = par.names,
                        par.initial = par.initial,
                        state.names = state.names,
                        basis.list = b_spline_basis_L, lambda = 1e2, controls = c(max_eval = 40))
        
        
        mean_observed <- mean(L_centered)
        
        total_sum_squares <- sum((L_centered - mean_observed)^2)
        
        residual_sum_squares <- sum((L_centered - fitted_values)^2)
        
        R_squared <- 1 - (residual_sum_squares / total_sum_squares)
        
        params_estimated = result$structural.par
        initial_state <- c(X = L_centered[1])
        times = tt
        sim_results <- ode(y = initial_state, times = times, func = ode_system, parms = params_estimated)
        
        plot_name = paste(glacier,'-',R_squared)
        plot(tt, L_centered, type='l', col='blue', xlab='Time', ylab='L', main=plot_name)
        lines(tt, sim_results[,2], col='red') # Overlay the modeled L
        legend('topright', legend=c('Original L', 'Modeled L'), col=c('blue', 'red'), lty=1)
        
        # Calculate residuals
        residuals <- L_centered - fitted_values
        
        # Plot the residuals
        plot(tt, residuals, xlab = "Time", ylab = "Residuals", main = plot_name, pch = 20, col = 'blue')
        print(glacier)
        print(result$structural.par)



    }
}