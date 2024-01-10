key = "new_key"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/01_prepare_dem/src_main.R"))
main.function_01_prepare_dem(key, root_dir)
