key = "$KEY"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/$xx/src_main.R"))
main.function_$xx(key, root_dir)
