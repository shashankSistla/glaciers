key = "debug"

library(rprojroot)

# Define the project root and files
root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/constants.R"))

key_dir = paste0(root_dir, "/output/",key)

glacier_list_path <- paste0(key_dir, "/key_glacier_list.R")

# Read and evaluate the original list of glaciers
original_glaciers <- local({
  source(glacier_list_path)
  glacier_list
})

# Function to update the glacier list for a single glacier and run steps
run_steps_for_glacier <- function(glacier) {
  single_glacier_list <- paste("glacier_list <- c('", glacier, "')", sep="")
  writeLines(single_glacier_list, glacier_list_path)
  
  # Run the script that processes each step
  source(paste0(root_dir, "/output/", key, "/run_all_steps.R"))
}

# Iterate over each glacier, update the list, and run the steps
for (glacier in original_glaciers) {
  run_steps_for_glacier(glacier)
}

# Optionally, restore the original glacier_list file
writeLines(paste("glacier_list <- c('", paste(original_glaciers, collapse = "', '"), "')", sep=""), glacier_list_path)
