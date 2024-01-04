main.function_01_prepare_dem <- function(key, root_dir){

    # STEP NAME
    step_name = "01_prepare_dem"

    # LIBRARIES 
    library(rprojroot)
    library(sp)
    library(terra)
    library(raster)

    # SOURCE
    source(paste0(root_dir,"/config.R"))
    source(paste0(root_dir, "/src/steps_code/01_prepare_dem/functions.R"))
    source(paste0(root_dir, "/src/base_functions.R"))
    source(paste0(root_dir, "/output/",key,"/01_prepare_dem/glacier_list.R"))


    # PATH MANAGEMENT
    dem_dir_path = config$dem_dir_path
    landsat_images_dir_path = config$landsat_images_dir_path
    work_dir_path = config$work_dir_path
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")

    glacier_count = 1
    for(glacier in glacier_list){

        #Logging progress
        glacier_count = progress(glacier, glacier_count)

        # Select current glacier's directory
        glacier_landsat_images_dir = paste0(landsat_images_dir_path, "/", glacier,"/")

        # Select all .tif files from directory
        filenames = list.files(glacier_landsat_images_dir)
        filenames = filenames[grepl(".tif", filenames, fixed = TRUE)]

        # Extract extents of each landsat image
        setwd(glacier_landsat_images_dir)
        raw_image_extents <- t(sapply_with_progress(filenames, extract_image_extent))

        # Add column names (xmin, xmax, ymin, ymax) and row names (the dates corresponding to the image) to the raw_image_extents matrix
        colnames(raw_image_extents) <- c("xmin", "xmax", "ymin", "ymax")
        rownames(raw_image_extents) <- sapply(strsplit(rownames(raw_image_extents), "_", fixed = TRUE), `[`, 2)

        # Format dates, and add filename to dataframe
        raw_image_extents <- tibble::rownames_to_column(as.data.frame(raw_image_extents), "date")
        raw_image_extents[,"filename"] <- filenames

        # Select one landsat image whose xmin is the mode amongst all images
        mode_xmin = as.double(calculate_mode(raw_image_extents[,"xmin"]))
        mode_xmin_images <- raw_image_extents[raw_image_extents[,"xmin"] == mode_xmin,] 
        sampled_image <- mode_xmin_images[1,]

        # Get paths of corresponding dem and landsat image
        dem_path = paste0(dem_dir_path, "/", glacier, "_NASADEM.tif")
        landsat_image_path = paste0(glacier_landsat_images_dir, "/", sampled_image$filename)

        # Read dem and landsat image
        landsat_image = stack(landsat_image_path)
        dem = readDEM(dem_path)

        # Possible edge case - If the DEM is all zero because it is above 60 degrees latitude it uses the GMTED DSM from the google drive folders
        if(all(as.matrix(!is.na(dem)) == 0)){
            print("Edge case detected")
        #     dem = readDEM("USGS_GMTED2010.tif")
        }

        # Project the DEM to the landsat image's CRS
        dem = projectRaster(dem, landsat_image)

        # Write the dem to output directory
        raster_filename = paste0(output_dir,"/output/", glacier,"_dem")
        writeRaster(dem, filename=raster_filename, format="GTiff", overwrite=TRUE)

    }
}



