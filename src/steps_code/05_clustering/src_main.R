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
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
    step_04_output_dir = paste0(root_dir, "/output/", key, "/04_candidate_paths/output/")
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir = paste0(output_dir, "/output")

    # LOAD NECESSARY FILES AND PARAMS
    distPerYear = params$step_5$distPerYear
    n_paths = params$step_5$n_paths
    optics_min_pts = params$step_5$optics_min_pts
    optics_eps = params$step_5$optics_eps
    should_plot = params$step_5$step_5_plot

    print("min pts is")
    print(optics_min_pts)

    for(glacier in glacier_list){

        # Read output from previous steps
        outs = readRDS(paste0(step_04_output_dir, glacier, "_outs.rds"))
        tt = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        ss = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        sSmooth = readRDS(paste0(step_04_output_dir, glacier, "_sSmooth.rds"))

        
        # Generate paths using multivariate normal sampling
        generated_paths <- list()
        for(i in seq_len(n_paths)){
            mean_vector <- outs[[paste0("out", i)]]$pred
            covariance_matrix <- calculate_covariance_matrix(i, outs) 
            generated_paths[[paste0("path", i, "_original")]] <- mean_vector

            new_samples <- mvrnorm(n = 4, mu = as.vector(mean_vector), Sigma = covariance_matrix)
            for (j in 1:nrow(new_samples)) {
                generated_paths[[paste0("path", i, "_sample", j)]] <- new_samples[j,]
            }
        }

        # Make the paths a matrix
        all_paths <- do.call(rbind, generated_paths)

        # Apply OPTICS and extract clusters
        optics_result <- optics(all_paths,  minPts = optics_min_pts, eps = optics_eps)
        res <- extractDBSCAN(optics_result, eps_cl)
        kc = res$cluster

        # Convert the paths into a list
        all_path_list <- lapply(seq_len(ncol(t(all_paths))), function(i) {
            pilot_path <- t(all_paths)[,i]
            return(pilot_path)
        })

        # Output directory management
        create_directory(output_dir, glacier)
        plots_dir = paste0(output_dir,"/", glacier)

        if(should_plot){

        # Reachability Plot
        plot_name_reachability = paste0(plots_dir, "/",glacier,"_reachability.png")
        png(plot_name_reachability)
        reachabilityPlot(glacier, res)
        dev.off()

        # Clustered paths plot
        plot_name_clustered = paste0(plots_dir, "/",glacier,"_clustered.png")
        png(plot_name_clustered)
        plot_clustered_paths(glacier, sSmooth$dd1,tt,ss,all_path_list,kc)
        dev.off()

        # Representative path for each cluster with mean and std plot
        plot_name_mean_std = paste0(plots_dir, "/",glacier,"_mean_std.png")
        png(plot_name_mean_std)
        plot_clustered_mean_std(glacier, sSmooth$dd1, tt, ss, all_path_list, kc, col_list)
        dev.off()
        }
    }
    }