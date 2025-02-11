main.function_05_clustering <- function(key, root_dir){

    # STEP_NAME
    step_name = "05_clustering"

    # LIBRARIES
    library(splines)
    library(fields)
    library(grDevices)
    library(scales)
    library(magick)
    library(stats)
    library(dbscan)
    library(cluster)
    library(MASS)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/05_clustering/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/05_clustering/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/keys/", key,".R"))

    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    landsat_images_dir_path = config$landsat_images_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    step_04_output_dir = paste0(root_dir, "/output/", key, "/04_candidate_paths/output/")
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir_path = paste0(output_dir, "/output")

    # LOAD NECESSARY FILES AND PARAMS
    distPerYear = params$step_5$distPerYear
    n_paths = params$step_5$n_paths
    optics_min_pts = params$step_5$optics_min_pts
    optics_eps = params$step_5$optics_eps
    should_plot = params$step_5$step_5_plot

    # Output directory management
    create_directory(output_dir_path, "plots")
    plots_dir = paste0(output_dir_path,"/", "plots")


    glacier_count = 1
    for(glacier in glacier_list){
        #Logging progress
        glacier_count = progress(glacier, glacier_count)
        # Read output from previous steps
        candidate_paths = readRDS(paste0(step_04_output_dir, glacier, "_candidate_paths.rds"))
        path_costs = readRDS(paste0(step_04_output_dir, glacier, "_path_costs.rds"))
        tt = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        ss = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        sSmooth = readRDS(paste0(step_04_output_dir, glacier, "_sSmooth.rds"))

        # Make the paths a matrix
        all_paths_indices <- do.call(rbind, candidate_paths)
        all_paths <- list()
        for (i in 1:nrow(all_paths_indices)) {
            all_paths[[i]] <- ss[all_paths_indices[i, ]]
        }

        all_paths <- do.call(rbind, all_paths)

        normalized_all_paths <- all_paths / sqrt(dim(all_paths)[2])

        # Apply OPTICS and extract clusters
        optics_result <- optics(normalized_all_paths,  minPts = optics_min_pts, eps = optics_eps)

        eps_cl = max(ss)*0.05
        res <- extractDBSCAN(optics_result, eps_cl)
        kc = res$cluster
        print(kc)

        print("dim of all paths is")
        print(dim(all_paths))

        print("ssmax is")
        print(max(ss))
        print("number of timestamps is")
        print(dim(all_paths)[2])

        print("eps_cl is")
        print(eps_cl)

        # Convert the paths into a list
        all_path_list <- lapply(seq_len(ncol(t(all_paths))), function(i) {
            pilot_path <- t(all_paths)[,i]
            return(pilot_path)
        })

        
        # Calculating mean and std curves
        # curves_list <- calculate_mean_std_curves(all_path_list, kc)
        # curves_list_filename = paste0(output_dir_path, "/",glacier,"_curves_list.rds")
        # saveRDS(curves_list, file = curves_list_filename)
        # print(length(curves_list[[1]]$mean))


        min_cost_indices <- sapply(unique(kc), function(cluster) {
        cluster_indices = which(kc == cluster)  # Indices of paths in this cluster
        cluster_costs = path_costs[cluster_indices]  # Costs of paths in this cluster
        min_index = cluster_indices[which.max(cluster_costs)]  # Index of the min cost path in the original vector (not sure why it's max but this is correct)
        return(min_index)
        })

        min_cost_indices_filename = paste0(output_dir_path, "/",glacier,"_min_cost_indices.rds")
        saveRDS(min_cost_indices, file = min_cost_indices_filename)

        if(should_plot){


        # Clustered paths plot, must highlight path with least cost
        plot_name_clustered = paste0(plots_dir, "/",glacier,"_clustered.png")
        png(plot_name_clustered)
        plot_clustered_paths(glacier, sSmooth$dd1,tt,ss,all_path_list,kc, min_cost_indices)
        dev.off()


        # Reachability Plot
        plot_name_reachability = paste0(plots_dir, "/",glacier,"_reachability.png")
        png(plot_name_reachability)
        reachabilityPlot(glacier, res)
        dev.off()

        # Representative path for each cluster with mean and std plot
        # plot_name_mean_std = paste0(plots_dir, "/",glacier,"_mean_std.png")
        # png(plot_name_mean_std)
        # plot_clustered_mean_std(glacier, sSmooth$dd1, tt, ss, curves_list, col_list)
        # dev.off()
        

        }

        date_strings = lapply(tt, convertDecimalYearToDate)
        
        landsat_image_paths <- list()

        for(i in 1:length(tt)) {
            date_string = convertDecimalYearToDate(tt[[i]])
            landsat_image_path = paste0(landsat_images_dir_path, glacier, "_", date_string, "_L5_T1_TOA.tif")
            landsat_image_paths[[i]] <- landsat_image_path
        }

        print("Reading landsat images")
        #landsatReadOutput = landsatRead(landsat_image_paths)

# landsat_image_paths now contains all the generated paths

    }
    }