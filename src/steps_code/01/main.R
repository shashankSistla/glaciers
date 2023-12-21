## Config file should read joined.csv

## Load necessary functions

## Read glaciers.txt

## For loop through all glaciers
## get corresponding x and y

## Handle img_coord_rangge_logic

## Get mode

## Read the DEM file

## if DEM is zero for some reason, fallback to default tif file

## Project dem to same CRS as Landsat image

## write the DEM to disk for next step to read

## Add logging

library(rprojroot)
library(sp)
library(terra)
library(raster)



root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)
step_dir <- "/steps/01_prepare_dem/"
step_2_dir <- "/steps/02_prepeare"



joined_csv_path <- paste0(root_dir, "/bin/joined.csv")

print(root_dir)
print(step_dir)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, step_dir, "glacier_list.R"))
source(paste0(root_dir, step_dir, "functions.R"))
source(paste0(root_dir, "/src/base_functions.R"))


args <- commandArgs(trailingOnly = TRUE)
key = parse_key(args)

dem_dir_path = config$dem_dir_path
landsat_images_dir_path = config$landsat_images_dir_path
work_dir_path = config$work_dir_path

joined <- read.csv(joined_csv_path, header = T)

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

    x = joined[which(joined$glac_id == glacier),]$x
    y = joined[which(joined$glac_id == glacier),]$y

    initial.coord=initial_to_UTM(x,y, crs(landsatImage))

    dem = projectRaster(dem, landsatImage)
    dem.extent = extent(dem)
    print(class(dem))

    create_directory(output_path, key)

    output_path = paste0(work_dir_path, "/output/",key, )
    
    writeRaster(dem, filename=output_path, format="GTiff", overwrite=TRUE)
    # Above 3 lines most likely for the next step
}




