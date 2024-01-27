key = "latest"
library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/steps_code/03_extract_IP/src_main.R"))
main.function_03_extract_IP(key, root_dir)
