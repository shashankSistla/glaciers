main.function_07_fda <- function(key, root_dir){
# STEP_NAME
    step_name = "07_fda"

    # LIBRARIES
    library(splines)
    library(mgcv)
    library(scales)
    library(fields)
    library(grDevices)
    library(numDeriv) # For jacobian
    library(dplyr)
    library(zoo) # For rollapply
    #library(base)
    library(pracma)
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
    step_06_output_dir = paste0(root_dir, "/output/", key, "/06_smoothing/output/")

    output_dir = paste0(work_dir_path, "output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir_path = paste0(output_dir, "/output")
    create_directory(output_dir_path, "plots")
    plots_dir = paste0(output_dir_path,"/", "plots")
    env_data_dir = paste0(work_dir_path, "data/GEE/")

    create_directory(output_dir_path, "plots")
    plots_dir = paste0(output_dir_path,"/", "plots")

    col_list = c('black','red','yellow','green', 'blue','pink','brown', 'purple', 'cyan', 'magenta', 'grey', 'darkgreen', 'darkblue', 'lightblue', 'magenta','magenta','magenta','magenta')


    
    glacier_count = 1
    for(glacier in glacier_list){
        glacier_count = progress(glacier, glacier_count)


        tt = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        ss = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        #sSmooth = readRDS(paste0(step_06_output_dir,glacier,"_sSmooth.rds"))
        candidate_paths = readRDS(paste0(step_04_output_dir,glacier,"_candidate_paths.rds"))
        min_cost_indices = readRDS(paste0(step_05_output_dir,glacier,"_min_cost_indices.rds"))
        knotbuffer = 1

        smooth_paths = readRDS(paste0(step_06_output_dir,glacier,"_smoothened_paths.rds"))
        
        # Read environmental data
       env_data <- read.csv(paste0(env_data_dir,glacier,"_env.csv"))
        env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")

        temp_above_273 <- env_data %>%
        filter(Temperature_2m > 273) %>%
        dplyr::select(Year, Month, Temperature_2m)

        overall_mean_summer_temperature <- temp_above_273 %>%
        summarise(Mean = mean(Temperature_2m, na.rm = TRUE)) %>%
        pull(Mean)

        overall_mean_precipitation <- mean(env_data$Total_Precipitation, na.rm = TRUE)

        env_data <- env_data %>%
        mutate(
            IsHighTempMonth = paste(Year, Month) %in% paste(temp_above_273$Year, temp_above_273$Month)
        )

        # Use raw values of environmental data
        env_data <- env_data %>%
        mutate(
            PrecipitationDeparture = Total_Precipitation - overall_mean_precipitation,
            SummerTemperatureDeparture = ifelse(IsHighTempMonth, Temperature_2m - overall_mean_summer_temperature, NA)
        )

        env_year <- env_data %>%
        group_by(Year) %>%
        summarise(
            YearWiseTemperature = mean(SummerTemperatureDeparture, na.rm = TRUE),
            YearWisePrecip = mean(PrecipitationDeparture, na.rm = TRUE)
        )

        # Initialize variables to track y-limits
        all_yearly_avg_L_deviated <- numeric()
        all_fitted_values_lm <- numeric()

        # Loop through all paths to find y-limits
        for (path_index in 1:length(smooth_paths)) {
        fit = smooth_paths[[path_index]]$fit
        smooth = fit$smooth[[1]]
        selected_candidate_paths = candidate_paths[min_cost_indices]
        path = selected_candidate_paths[[path_index]]

        # Process path data using GAM and FDA
        res_path = gam_to_fda_with_se(fit, tt, range(tt))
        coef_mat = res_path$coefficients_matrix
        bspline_basis = res_path$bspline_basis

        # Evaluate L and dL/dt at fine intervals
        fine_t <- seq(from = min(tt), to = max(tt), by = 1/12)  # Monthly intervals
        fine_L <- eval.basis(fine_t, bspline_basis) %*% coef_mat %*% fit$coefficients
        fine_dL_dt <- (eval.basis(fine_t, bspline_basis, 1) %*% coef_mat) %*% fit$coefficients

        # Calculate L_deviated
        L_initial <- fine_L[1]
        L_deviated <- fine_L - L_initial

        # Create a numeric vector representing the year for each fine_t
        fine_years <- floor(fine_t)

        # Compute yearly averages for L_deviated and dL/dt using aggregate
        yearly_avg_L_deviated <- aggregate(L_deviated, by = list(fine_years), mean)$V1
        yearly_avg_dL_dt <- aggregate(fine_dL_dt, by = list(fine_years), mean)$V1

        # Extract the standard errors from the GAM model
        pred_gam <- predict(fit, se.fit = TRUE)
        standard_errors_gam <- pred_gam$se.fit

        # Extract years from the time points
        years <- as.integer(format(tt))

        # Aggregate the standard errors on a yearly basis
        yearly_se_gam <- aggregate(standard_errors_gam, by = list(years), FUN = mean)$x
        avg_years <- unique(years)

        # Fit the regression model including the intercept term
        regression_result <- lm(yearly_avg_dL_dt ~ L + P + T, data = data.frame(
            L = yearly_avg_L_deviated,
            P = env_year$YearWisePrecip,
            T = env_year$YearWiseTemperature
        ))

        # Predict fitted values and standard errors
        predictions <- predict(regression_result, se.fit = TRUE)
        fitted_values_lm <- predictions$fit
        standard_errors_lm <- predictions$se.fit

        # Combine standard errors
        combined_standard_errors <- sqrt(yearly_se_gam^2 + standard_errors_lm^2)

        # Collect all values to find y-limits
        all_yearly_avg_L_deviated <- c(all_yearly_avg_L_deviated, yearly_avg_L_deviated)
        all_fitted_values_lm <- c(all_fitted_values_lm, fitted_values_lm)
        }

        # Determine y-limits
        y_min <- min(c(all_yearly_avg_L_deviated, all_fitted_values_lm))
        y_max <- max(c(all_yearly_avg_L_deviated, all_fitted_values_lm))

        plot_name_smoothened = paste0(plots_dir, "/",glacier,"_regression.png")
        png(plot_name_smoothened)
        # Initialize plot for all paths
        plot(NULL, xlim = range(tt), ylim = c(y_min, y_max), type = "n", main = "Actual vs Fitted Path Values with SE for All Paths", xlab = "Year", ylab = "Path")

        # Loop through all paths to plot
        for (path_index in 1:length(smooth_paths)) {
        fit = smooth_paths[[path_index]]$fit
        smooth = fit$smooth[[1]]
        selected_candidate_paths = candidate_paths[min_cost_indices]
        path = selected_candidate_paths[[path_index]]

        # Process path data using GAM and FDA
        res_path = gam_to_fda_with_se(fit, tt, range(tt))
        coef_mat = res_path$coefficients_matrix
        bspline_basis = res_path$bspline_basis

        # Evaluate L and dL/dt at fine intervals
        fine_t <- seq(from = min(tt), to = max(tt), by = 1/12)  # Monthly intervals
        fine_L <- eval.basis(fine_t, bspline_basis) %*% coef_mat %*% fit$coefficients
        fine_dL_dt <- (eval.basis(fine_t, bspline_basis, 1) %*% coef_mat) %*% fit$coefficients

        # Calculate L_deviated
        L_initial <- fine_L[1]
        L_deviated <- fine_L - L_initial

        # Create a numeric vector representing the year for each fine_t
        fine_years <- floor(fine_t)

        # Compute yearly averages for L_deviated and dL/dt using aggregate
        yearly_avg_L_deviated <- aggregate(L_deviated, by = list(fine_years), mean)$V1
        yearly_avg_dL_dt <- aggregate(fine_dL_dt, by = list(fine_years), mean)$V1

        # Extract the standard errors from the GAM model
        pred_gam <- predict(fit, se.fit = TRUE)
        standard_errors_gam <- pred_gam$se.fit

        # Extract years from the time points
        years <- as.integer(format(tt))

        # Aggregate the standard errors on a yearly basis
        yearly_se_gam <- aggregate(standard_errors_gam, by = list(years), FUN = mean)$x
        avg_years <- unique(years)

        # Fit the regression model including the intercept term
        regression_result <- lm(yearly_avg_dL_dt ~ L + P + T, data = data.frame(
            L = yearly_avg_L_deviated,
            P = env_year$YearWisePrecip,
            T = env_year$YearWiseTemperature
        ))

  # Predict fitted values and standard errors
  predictions <- predict(regression_result, se.fit = TRUE)
  fitted_values_lm <- predictions$fit
  standard_errors_lm <- predictions$se.fit

  # Combine standard errors
  combined_standard_errors <- sqrt(yearly_se_gam^2 + standard_errors_lm^2)

  # Plot actual vs fitted path values with combined standard errors
  lines(avg_years, yearly_avg_L_deviated, col =  col_list[path_index], lty = 1)
  lines(avg_years, fitted_values_lm, col = col_list[path_index], lty = 2)
  #polygon(c(avg_years, rev(avg_years)), c(fitted_values_lm + combined_standard_errors, rev(fitted_values_lm - combined_standard_errors)), col = rgb(path_index / length(file), 0, 1 - path_index / length(file), 0.2), border = NA)

  # Extract regression coefficients and compute additional coefficients
  regression_coefficients <- coef(regression_result)
  alpha <- round(regression_coefficients["P"], 2)
  beta <- round(regression_coefficients["T"], 2)
  gamma <- round(regression_coefficients["(Intercept)"], 2)
  tau <- round(1 / regression_coefficients["L"], 2)

  # Add text with alpha, beta, gamma, and tau values
  # Add text with alpha, beta, gamma, and tau values at the bottom left
  # Add text with alpha, beta, gamma, and tau values at the bottom left
  text(x = min(tt), y = y_min + path_index * 0.1 * (y_max - y_min), labels = paste("Path", path_index, ": α=", alpha, ", β=", beta, ", γ=", gamma, ", τ=", tau), col = col_list[path_index], pos = 4)
  
}

# Add legend
legend("topright", legend = paste("Path", 1:length(smooth_paths)), col = col_list[1:(length(smooth_paths))], lwd = 2)

dev.off()

    }
}