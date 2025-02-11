main.function_04_candidate_paths <- function(key, root_dir){

    # STEP NAME
    step_name = "04_candidate_paths"

    # LIBRARIES 
    library(fda)
    library(splines)
    library(mgcv)
    library(MASS)
    library(fields)
    library(grDevices)
    library(scales)
    library(matrixStats)
    library(phonTools)
    library("customizedTraining")
    library('matlab')


    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/04_candidate_paths/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/04_candidate_paths/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/keys/", key,".R"))


    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")

    # LOAD NECESSARY FILES AND PARAMS
    #distPerYear = params$step_4$distPerYear
    n_paths = params$step_4$n_paths

    FV = get_FV_joined_file()
    dist_list <- seq(50, 200, by = 50)
    glacier_count = 1
    for(glacier in glacier_list){
        
        #Logging progress
        glacier_count = progress(glacier, glacier_count)

        #Get arc length in meters between each path point)
        ts_intensity = readRDS(paste0(step_03_output_dir, glacier, "_ts_intensity.rds"))
        dates_cut = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))

        create_directory(paste0(output_dir,"/output/"), "plots")
        plot_path = paste0(output_dir, "/output/plots/", glacier,"_candidate_paths.png")

        # If I change the original arc length function, this step will be unnecessary
        al = c(0,cumsum(rep(al[length(al)]/(ncol(ts_intensity)-1), (ncol(ts_intensity)-1))))

        #Smoothen each time series as a spline (obs vs tt)
        knotbuffer = 2
        knotbuffer = min(4, knotbuffer) # this is set to be 4 at maximum because the knots may be too little when setting spacing too large
        tSmooth = time_series_spline_smooth(ts_intensity, dates_cut, number_of_knots = round(diff(range(dates_cut))/knotbuffer))

        #Smoothen the spatial components
        sSmooth = spatial_smooth(tSmooth, al, number_of_knots = min(round(length(al)/4)+4, 35+4))

        pilot_path_output = pilot_path_algorithm(sSmooth$dd1,dates_cut,al,glacier,invert=1 ,dist_list = dist_list, n_paths = n_paths)
        term_paths = pilot_path_output$term_paths
        path_costs = pilot_path_output$path_costs

        #TODO structure outs better
        outs_filename = paste0(output_dir, "/output/", glacier,"_candidate_paths.rds")
        saveRDS(term_paths, file = outs_filename)

        sSmooth_filename = paste0(output_dir, "/output/", glacier,"_sSmooth.rds")
        saveRDS(sSmooth, file = sSmooth_filename)

        path_costs_filename = paste0(output_dir, "/output/", glacier, "_path_costs.rds")
        saveRDS(path_costs, file = path_costs_filename)
        
        values_list <- lapply(term_paths, function(indices) {
            al[indices]
        })


        plot_candidate_paths(glacier, al, dates_cut, values_list, dist_list, sSmooth$dd1, n_paths, path_costs, plot_path)
    }

    
}