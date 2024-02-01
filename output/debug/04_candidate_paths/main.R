key = "debug"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/04_candidate_paths/src_main.R"))
main.function_04_candidate_paths(key, root_dir)
