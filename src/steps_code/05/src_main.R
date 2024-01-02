main.function_05 <- function(key, root_dir){

    step_name = "05_clustering"

    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/05_clustering/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/05/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/input/", key,".R"))

    library(splines)
    library(fields)
    library(grDevices)
    library(scales)
    library(magick)
    library(stats)
    library(dbscan)
    library(cluster)
    library(MASS)

    work_dir_path = config$work_dir_path
    step_03_output_dir = paste0(root_dir, "/output/", key, "/03_process_landsat/output/")
    step_04_output_dir = paste0(root_dir, "/output/", key, "/04_terminus/output/")

    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")
    output_dir = paste0(output_dir, "/output")

    distPerYear = params$step_5$distPerYear

    for(glacier in glacier_list){
        outs = readRDS(paste0(step_04_output_dir, glacier, "_outs.rds"))
        tt = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
        ss = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
        sSmooth = readRDS(paste0(step_04_output_dir, glacier, "_sSmooth.rds"))

    generated_paths <- list()
    n_paths = 20

    create_directory(output_dir, glacier)
    for(i in seq_len(n_paths)){
        mean_vector <- outs[[paste0("out", i)]]$pred
        covariance_matrix <- calculate_covariance_matrix(i, outs) 
        generated_paths[[paste0("path", i, "_original")]] <- mean_vector

        new_samples <- mvrnorm(n = 4, mu = as.vector(mean_vector), Sigma = covariance_matrix)
        for (j in 1:nrow(new_samples)) {
            generated_paths[[paste0("path", i, "_sample", j)]] <- new_samples[j,]
        }

        all_paths <- do.call(rbind, generated_paths)
        optics_result <- optics(all_paths,  minPts = 2,eps = 50000)

        res <- extractDBSCAN(optics_result, eps_cl)
        

        plots_dir = paste0(output_dir,"/", glacier)

        plot_name_reachability = paste0(plots_dir, "/",glacier,"_reachability.png")
        png(plot_name_reachability)
        reachabilityPlot(glacier, res)
        dev.off()

        kc = res$cluster

        all_path_list <- lapply(seq_len(ncol(t(all_paths))), function(i) {
            pilot_path <- t(all_paths)[,i]
            return(pilot_path)
        })

        plot_name_clustered = paste0(plots_dir, "/",glacier,"_clustered.png")
        png(plot_name_clustered)
        plot_clustered_paths(glacier, sSmooth$dd1,tt,ss,all_path_list,kc)
        dev.off()

        plot_name_mean_std = paste0(plots_dir, "/",glacier,"_mean_std.png")
        png(plot_name_mean_std)
        plot_clustered_mean_std(glacier, sSmooth$dd1, tt, ss, all_path_list, kc, col_list)
        dev.off()
    }
    }
}