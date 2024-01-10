library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir,"/config.R"))

landsat_images_dir_path = config$landsat_images_dir_path

glaciers <- list.dirs(landsat_images_dir_path, full.names = FALSE, recursive = FALSE)

glaciers_list_string <- paste("glacier_list <- c(", paste0("'", glaciers, "'", collapse = ", "), ")", sep = "")

glacier_list_main_file = paste0(root_dir, "/glacier_list_main.R")
cat(glaciers_list_string, file = glacier_list_main_file)
 cat("\n", file = glacier_list_main_file, append = TRUE)