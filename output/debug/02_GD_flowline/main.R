key = "debug"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/02_GD_flowline/src_main.R"))
main.function_02_GD_flowline(key, root_dir)
