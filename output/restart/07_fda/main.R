key = "restart"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/07_fda/src_main.R"))
main.function_07_fda(key, root_dir)
