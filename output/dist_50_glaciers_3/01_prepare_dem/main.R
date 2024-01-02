key = "dist_50_glaciers_3"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/01/src_main.R"))
main.function_01(key, root_dir)
