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
    distPerYear = params$step_4$distPerYear
    n_paths = params$step_4$n_paths

    FV = get_FV_joined_file()

    glacier_count = 1
    for(glacier in glacier_list){
        
        #Logging progress
        glacier_count = progress(glacier, glacier_count)

        #Get arc length in meters between each path point
        ts_intensity = readRDS(paste0(step_03_output_dir, glacier, "_ts_intensity.rds"))
        dates_cut = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))

        create_directory(paste0(output_dir,"/output/"), "plots")
        create_directory(paste0(output_dir,"/output/plots"), glacier)
        plot_path = paste0(output_dir, "/output/plots/", glacier,"/")

        # If I change the original arc length function, this step will be unnecessary
        al = c(0,cumsum(rep(al[length(al)]/(ncol(ts_intensity)-1), (ncol(ts_intensity)-1))))
        term_paths = terminus(glacier, ts_intensity, al, dates_cut, plot = TRUE, distPerYear = distPerYear,linefit = 0,temporal = 1, direc = plot_path, meas = meas, knotbuffer = 2, n_paths = n_paths)

        #TODO structure outs better
        outs_filename = paste0(output_dir, "/output/", glacier,"_outs.rds")
        saveRDS(term_paths$outs, file = outs_filename)

        sSmooth_filename = paste0(output_dir, "/output/", glacier,"_sSmooth.rds")
        saveRDS(term_paths$sSmooth, file = sSmooth_filename)


    }
}