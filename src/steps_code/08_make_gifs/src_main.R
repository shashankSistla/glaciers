main.function_08_make_gifs <- function(key, root_dir){

  # STEP NAME
  step_name = "08_make_gifs"

  # LIBRARIES 
  options(repos = c(CRAN = "https://cloud.r-project.org/"))

  library(animation)
  library(raster)
  library(fields)
    library(grDevices)
    library(scales)
  # SOURCE
  source(paste0(root_dir,"/config.R"))
  source(paste0(root_dir, "/output/",key,"/08_make_gifs/glacier_list.R"))
  source(paste0(root_dir, "/src/steps_code/08_make_gifs/functions.R"))
  source(paste0(root_dir, "/src/base_functions.R"))
  source(paste0(root_dir, "/keys/", key,".R"))

  work_dir_path = config$work_dir_path
  landsat_images_dir_path = config$landsat_images_dir_path

  step_02_output_dir = paste0(root_dir, "/output/", key, "/02_GD_flowline/output/")
  step_03_output_dir = paste0(root_dir, "/output/", key, "/03_extract_IP/output/")
  step_04_output_dir = paste0(root_dir, "/output/", key, "/04_candidate_paths/output/")
  step_05_output_dir = paste0(root_dir, "/output/", key, "/05_clustering/output/")

  output_dir_path = paste0(work_dir_path, "/output/",key,"/",step_name)
  create_directory(output_dir_path, "output")
  glacier_count = 1
  for(glacier in glacier_list){
      #Logging progress
      glacier_count = progress(glacier, glacier_count)

      # Loading parallel path co-ordinates
      coord.parallel = readRDS(paste0(step_02_output_dir, glacier, "_coord_parallel.rds"))
      initial.coord = readRDS(paste0(step_02_output_dir, glacier, "_initial_coord.rds"))


      # Loading step 3 data
      indices_to_remove = readRDS(paste0(step_03_output_dir, glacier, "_indices_to_remove.rds"))
      outlier_indices = readRDS(paste0(step_03_output_dir, glacier, "_outlier.rds"))
      al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))
      dates_cut = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))


      candidate_paths = readRDS(paste0(step_04_output_dir,glacier,"_candidate_paths.rds"))
      min_cost_indices = readRDS(paste0(step_05_output_dir,glacier,"_min_cost_indices.rds"))

    
      # Getting landsat image filenames
      glacier_landsat_images_dir = paste0(landsat_images_dir_path, "/", glacier,"/")
      setwd(glacier_landsat_images_dir)
      filenames = list.files(getwd())
      filenames = filenames[grepl(".tif", filenames, fixed = TRUE)]

      landsatReadOutput = landsatRead(filenames)

      print("idnices to remove are")
      print(indices_to_remove)

      print("outliers are")
      print(outlier_indices)

    if(!identical(indices_to_remove, integer(0))){
      edited_landsat <-landsatReadOutput$landsatImgs[-indices_to_remove]
    } else {
      edited_landsat <- landsatReadOutput$landsatImgs
    }

    if(length(outlier_indices) != 0){
      edited_landsat <-edited_landsat[-outlier_indices]
    }


    print("Length of edited landsat is")
    print(length(edited_landsat))

    print("Length of dates cut is")
    print(dates_cut)

selected_candidate_paths = candidate_paths[min_cost_indices]
plot_path = paste0(output_dir_path,"/output/",glacier,".gif")

  animate_rgb(glacier, edited_landsat, initial.coord, coord.parallel ,selected_candidate_paths,al, plot_path)


  }
}
