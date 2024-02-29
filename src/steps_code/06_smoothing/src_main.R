main.function_06_smoothing <- function(key, root_dir){

    # STEP_NAME
    step_name = "06_smoothing"

    # LIBRARIES
    library(splines)
    library(mgcv)
    library(scales)
    library(fields)
    library(grDevices)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/06_smoothing/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/06_smoothing/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/keys/", key,".R"))

    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    step_04_output_dir = paste0(root_dir, "/output/", key, "/04_candidate_paths/output/")
    step_05_output_dir = paste0(root_dir, "/output/", key, "/05_clustering/output/")

    
    
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir_path = paste0(output_dir, "/output")

    # Output directory management
    create_directory(output_dir_path, "plots")
    plots_dir = paste0(output_dir_path,"/", "plots")

    for(glacier in glacier_list){

        dates_cut = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        sSmooth = readRDS(paste0(step_04_output_dir,glacier,"_sSmooth.rds"))
        candidate_paths = readRDS(paste0(step_04_output_dir,glacier,"_candidate_paths.rds"))
        min_cost_indices = readRDS(paste0(step_05_output_dir,glacier,"_min_cost_indices.rds"))

        knotbuffer = 1

        selected_candidate_paths = candidate_paths[min_cost_indices]
        

        smoothened_paths = list()
        for(i in 1:length(selected_candidate_paths)){
        smoothened_paths[[i]] = temporal_smooth(al, dates_cut, dd3 = sSmooth$dd3, term_path = selected_candidate_paths[[i]], knotsT = round(diff(range(dates_cut))/knotbuffer))
        }



        plot_name_smoothened = paste0(plots_dir, "/",glacier,"_smoothened.png")
        png(plot_name_smoothened)
        plot_smoothened_paths(glacier, sSmooth$dd1,dates_cut,al,smoothened_paths)
        dev.off()

        
        


    }

}