main.function_06_analysis <- function(key, root_dir){

    # STEP_NAME
    step_name = "06_analysis"
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("zoo")

    # LIBRARIES
    library(splines)
    library(fields)
    library(grDevices)
    library(scales)
    library(magick)
    library(stats)
    library(dbscan)
    library(cluster)
    library(zoo)
    library(MASS)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/06_analysis/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/06_analysis/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/keys/", key,".R"))

    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    step_05_output_dir = paste0(root_dir, "/output/", key, "/05_clustering/output/")
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir = paste0(output_dir, "/output")
    gee_dir = paste0(root_dir, "/data/GEE/")

    


    for(glacier in glacier_list){

        al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        env_file = paste0(gee_dir,glacier,'_env.csv')
        print(al)

        env_data <- read.csv(env_file)
        env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
        plot_path <- paste0(output_dir, '/', glacier, 'initial.png') 

        plot_path_precip <- paste0(output_dir, '/', glacier, '_precipitation.png')
        png(plot_path_precip)
        plot(env_data$Date, env_data$Total_Precipitation, type = "l", col = "blue", xlab = "Date", ylab = "Total Precipitation", main = paste0("Total Precipitation for ", glacier))
        model_precip <- lm(Total_Precipitation ~ Date, data = env_data)
        abline(model_precip, col = "green")
        dev.off()

        plot_path_temp <- paste0(output_dir, '/', glacier, '_temperature.png')
        png(plot_path_temp)
        plot(env_data$Date, env_data$Skin_Temperature, type = "l", col = "red", xlab = "Date", ylab = "Skin Temperature", main = paste0("Skin Temperature for ", glacier))
        model_temp <- lm(Skin_Temperature ~ Date, data = env_data)
        abline(model_temp, col = "orange")
        dev.off()

        env_data$RollingMean_Precip <- rollapply(env_data$Total_Precipitation, width = 12, FUN = mean, align = "center", partial = TRUE)
        plot_path_rolling_mean <- paste0(output_dir, '/', glacier, '_rolling_mean_precipitation.png')
        png(plot_path_rolling_mean)
        plot(env_data$Date, env_data$Total_Precipitation, type = "l", col = "blue", xlab = "Date", ylab = "Precipitation", main = paste0("Rolling Mean Precipitation for ", glacier))
        lines(env_data$Date, env_data$RollingMean_Precip, col = "red")
        dev.off()

        print("length of skin temperature")
        print(length(env_data$Skin_Temperature))

        print("lengh of total precipitation")
        print(length(env_data$Total_Precipitation))


        print(length(env_data$RollingMean_Precip))
        print(length(env_data$Total_Precipitation))

        curves_list = readRDS(paste0(step_05_output_dir,'/',glacier,'_curves_list.rds'))

        l_t <- curves_list[[1]]$mean

        l_t_lag1 <- stats::lag(zoo::zoo(l_t), -1)
        l_t_lag2 <- stats::lag(zoo::zoo(l_t), -2)
        l_t_lag3 <- stats::lag(zoo::zoo(l_t), -3)
        T_t_lag3 <- stats::lag(zoo::zoo(env_data$Skin_Temperature), -3)
        P_t_lag3 <- stats::lag(zoo::zoo(env_data$RollingMean_Precip), -3)

        data_for_regression <- data.frame(l_t, l_t_lag1, l_t_lag2, l_t_lag3, T_t_lag3, P_t_lag3)
        data_for_regression <- na.omit(data_for_regression)  # Remove NA values due to lagging

        regression_formula <- l_t ~ I(3 * l_t_lag1) - I(3 * l_t_lag2^2) + I(l_t_lag3^3) + T_t_lag3 + P_t_lag3
        model <- glm(regression_formula, data = data_for_regression)

        print(summary(model))

        plot_path_regression <- paste0(output_dir, '/', glacier, '_regression.png')
        png(plot_path_regression)
        plot(model)
        dev.off()


    }

}