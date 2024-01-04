main.function_02 <- function(key, root_dir){

    # STEP NAME    
    step_name = "02_GD_flowline"

    # LIBRARIES
    library(raster)
    library(matrixStats)
    library(stats)
    library(mgcv)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/output/",key,"/02_GD_flowline/glacier_list.R"))
    source(paste0(root_dir, "/src/steps_code/02/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/keys/", key,".R"))

    # PATH MANAGEMENT
    work_dir_path = config$work_dir_path
    step_01_output_dir = paste0(root_dir, "/output/", key, "/01_prepare_dem/output/")
    output_dir_path = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir_path, "output")

    # LOAD NECESSARY FILES AND PARAMS
    glaciers_start_coords = get_joined_file()
    plot_flowline = params$step_2$plot_flowline

    glacier_count = 1
    for(glacier in glacier_list){

        #Logging progress
        progress(glacier_count)

        # Read DEM
        glacier_dem_path = paste0(step_01_output_dir, glacier,"_dem.tif")
        dem = raster(glacier_dem_path)

        # Fetch starting co-ords of glacier and convert them to UTM
        x = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier),]$x
        y = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier),]$y

        initial.coord=initial_to_UTM(x,y, crs(dem))

        # Possible edge case - should check the validity of the case
        if(initial.coord[1] < extent(dem)[1] | initial.coord[1] > extent(dem)[2]){
            print("Edge case detected")
        }

        # Set step size and window multiplier based on the dimensions of DEM
        step.size = ((dim(dem)[1] + dim(dem)[2])/2)*30/1200
        window.multiplier = ifelse(floor(step.size/5) < 1, 1, ifelse((step.size/5 - floor(step.size/5)) < 0.5, floor(step.size/5), ceiling(step.size/5)))
        window.multiplier = ifelse(step.size/5 < 0.5, 0.5, window.multiplier)

        # Run the GD Flowline algorithm
        gdOutput = GD.linear.sample(dem, initial.coord, step.size = step.size, blocks = window.multiplier*c(30,40,50,60))

        #Get parallel paths
        coord.parallel = path_parallel(gdOutput$coord, dem)
        warnings = gdOutput$warning
        #flag.df[flag.df$GID == glacier, names(warnings)] = as.numeric(warnings)
        #write.csv(flag.df, "WarningMessages.csv", row.names = FALSE)

        #This next if statement gets rid of points that fall outside the boundaries of the DEM image
        #There are NAs on the edges of the DEM that means sometimes the flowline will extend into that section
        #So we remove them
        indx <- min(which(is.na(raster::extract(boundaries(dem), coord.parallel[,1:2])) | raster::extract(boundaries(dem), coord.parallel[,1:2]) > 0))
        if(indx > 0 & indx < Inf){
            coord.parallel = coord.parallel[1:(indx-1),]
        }

        coord_parallel_filename = paste0(output_dir_path,"/output/", glacier,"_coord_parallel.rds")
        saveRDS(coord.parallel, file = coord_parallel_filename)

        if(plot_flowline){
            flowline_plot_filename = paste0(output_dir, "/output/",glacier,"_flowline_dem.png")
            png(flowline_plot_filename)
            plot_flowline(initial.coord, coord.parallel)
            dev.off()
        }
    }
}