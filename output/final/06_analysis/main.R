key = "final"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/06_analysis/src_main.R"))
main.function_06_analysis(key, root_dir)
