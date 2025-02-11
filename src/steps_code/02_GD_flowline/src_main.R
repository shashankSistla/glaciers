main.function_02_GD_flowline <- function(key, root_dir) {
  step_name = "02_GD_flowline"
  library(raster)
  library(matrixStats)
  library(stats)
  library(magick)

  source(paste0(root_dir, "/config.R"))
  source(paste0(root_dir, "/output/", key, "/02_GD_flowline/glacier_list.R"))
  source(paste0(root_dir, "/src/steps_code/02_GD_flowline/functions.R"))
  source(paste0(root_dir, "/src/base_functions.R"))
  source(paste0(root_dir, "/keys/", key, ".R"))

  work_dir_path = config$work_dir_path
  step_01_output_dir = paste0(root_dir, "/output/", key, "/01_prepare_dem/output/")
  output_dir_path = paste0(work_dir_path, "/output/", key, "/", step_name)
  create_directory(output_dir_path, "output")
  create_directory(output_dir_path, "output/frames")

  glaciers_start_coords = get_joined_file()
  should_plot_flowline = params$step_2$should_plot_flowline 

  glacier_count = 1
  for (glacier in glacier_list) {
    glacier_count = progress(glacier, glacier_count)

    tryCatch({
      glacier_dem_path = paste0(step_01_output_dir, glacier, "_dem.tif")
      dem = raster(glacier_dem_path)

      x = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier), ]$x
      y = glaciers_start_coords[which(glaciers_start_coords$glac_id == glacier), ]$y

      initial.coord = initial_to_UTM(x, y, crs(dem))

      if (initial.coord[1] < extent(dem)[1] | initial.coord[1] > extent(dem)[2]) {
        print("Edge case detected")
      }

      step.size = ((dim(dem)[1] + dim(dem)[2]) / 2) * 30 / 1200
      window.multiplier = ifelse(floor(step.size / 5) < 1, 1, ifelse((step.size / 5 - floor(step.size / 5)) < 0.5, floor(step.size / 5), ceiling(step.size / 5)))
      window.multiplier = ifelse(step.size / 5 < 0.5, 0.5, window.multiplier)

      gdOutput = GD.linear.sample(dem, initial.coord, step.size = step.size, blocks = window.multiplier * c(30, 40, 50, 60), output_dir = paste0(output_dir_path, "/output/"), glacier = glacier)

      coord.parallel = path_parallel(gdOutput$coord, dem)
      warnings = gdOutput$warning

      indx <- min(which(is.na(raster::extract(boundaries(dem), coord.parallel[, 1:2])) | raster::extract(boundaries(dem), coord.parallel[, 1:2]) > 0))
      if (indx > 0 & indx < Inf) {
        coord.parallel = coord.parallel[1:(indx - 1), ]
      }

      coord_parallel_filename = paste0(output_dir_path, "/output/", glacier, "_coord_parallel.rds")
      saveRDS(coord.parallel, file = coord_parallel_filename)

      initial_coord_filename = paste0(output_dir_path, "/output/", glacier, "_initial_coord.rds")
      saveRDS(initial.coord, file = initial_coord_filename)

      if (should_plot_flowline) {
        flowline_plot_filename = paste0(output_dir_path, "/output/", glacier, "_flowline_dem.png")
        plot_dem(glacier, dem, initial.coord, coord.parallel, flowline_plot_filename)
      }
      
    }, error = function(e) {
      message(sprintf("Error in processing glacier %s for glacier: %s", glacier, e$message))
    })
  }
}
