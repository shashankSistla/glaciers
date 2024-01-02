main.function_01 <- function(key, root_dir){
step_name = "01_prepare_dem"

#TODO make a nice readFile function which throws good errors when trying to read a file
library(rprojroot)
library(sp)
library(terra)
library(raster)
print(root_dir)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/src/steps_code/01/functions.R"))
source(paste0(root_dir, "/src/base_functions.R"))
source(paste0(root_dir, "/output/",key,"/01_prepare_dem/glacier_list.R"))


dem_dir_path = config$dem_dir_path
landsat_images_dir_path = config$landsat_images_dir_path
work_dir_path = config$work_dir_path

glaciers_start_coords = get_joined_file()


for(glacier in glacier_list){
    print(glacier)
    exclusion_list <- c("meta_data", "TerminusPlot", 
                    paste0(glacier, "_terminus_prediction.csv"),
                    paste0(glacier, "_img_coord_range.csv"))


    glacier_landsat_images_dir = paste0(landsat_images_dir_path, "/", glacier,"/")
    print("glacier landasat images")
    # print(glacier_landsat_images)
    raw_image_length <- sum(!list.files(glacier_landsat_images_dir) %in% exclusion_list)

    setwd(glacier_landsat_images_dir)
    raw_image_extents <- t(sapply(list.files(getwd())[! list.files(getwd()) %in% exclusion_list], extract_image_extent))
    colnames(raw_image_extents) <- c("xmin", "xmax", "ymin", "ymax")
    rownames(raw_image_extents) <- unlist(strsplit(rownames(raw_image_extents),
                                                 split = "_", fixed = TRUE))[
                                                   seq(2,5*length(rownames(raw_image_extents)), 5)]
    
    raw_image_extents <- tibble::rownames_to_column(as.data.frame(raw_image_extents), "date")
    raw_image_extents[,"filename"] <- list.files(getwd())[! list.files(getwd()) %in% exclusion_list]

    mode_coord_imgs <- raw_image_extents[raw_image_extents[,"xmin"] == as.double(calculate_mode(raw_image_extents[,"xmin"])),] 
    sampled_img <- mode_coord_imgs[sample(rownames(mode_coord_imgs),1),]
    print(sampled_img)
    print("I come here")
    landsatImage <- stack(paste0(glacier_landsat_images_dir, "/", sampled_img$filename))
        print("hi")
        print(dem_dir_path)
    dem_path = paste0(dem_dir_path, "/", glacier, "_NASADEM.tif")
    print(dem_path)
    dem = readDEM(dem_path)

    x = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier),]$x
    y = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier),]$y

    initial.coord=initial_to_UTM(x,y, crs(landsatImage))

    dem = projectRaster(dem, landsatImage)
    dem.extent = extent(dem)
    print(class(dem))

    #TODO make output_dir logic neater
    output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
    create_directory(output_dir, "output")

    raster_filename = paste0(output_dir,"/output/", glacier,"_dem")
    initial_coord_filename = paste0(output_dir,"/output/", glacier, "_initial_coord.rds")
    
    print("Raster filename is")
    print(raster_filename)

    print("Initial co-ord filename is")
    print(initial_coord_filename)


    writeRaster(dem, filename=raster_filename, format="GTiff", overwrite=TRUE)
    saveRDS(initial.coord, file = initial_coord_filename)
    

        #COME BACK TO THIS
    # if(initial.coord[1] < extent(dem)[1] | initial.coord[1] > extent(dem)[2]){
    #   
    # }
    # 
    #If the DEM is all zero because it is above 60 degrees latitude it uses the GMTED DSM from the google drive folders
    # if(all(as.matrix(!is.na(dem)) == 0)){
    #   if((file.exists("USGS_GMTED2010.tif"))){
    #     dem = readDEM("USGS_GMTED2010.tif")
    #   }else{next}
    # }
}

}


