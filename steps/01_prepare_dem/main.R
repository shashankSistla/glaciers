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

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)
step_dir <- "/steps/01_prepare_dem/"

print("Root dir is")
print(root_dir)
print(step_dir)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, step_dir, "glacier_list.R"))
source(paste0(root_dir, step_dir, "functions.R"))


dem_path = config$dem_path
landsat_images_path = config$landsat_images_path
work_dir_path = config$work_dir_path

glaciers_start_coords = get_joined_file()


for(glacier in glacier_list){
    print(glacier)
}




