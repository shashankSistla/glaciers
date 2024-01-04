key = "final"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/05_clustering/src_main.R"))
main.function_05_clustering(key, root_dir)
