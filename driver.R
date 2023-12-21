# Run the script with command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No key name provided")
}
if (length(args) > 1){
    stop("Too many arguments provided")
}
 
key_name = args[1]

library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/src/base_functions.R"))
source(paste0(root_dir, "/src/constants.R"))


work_dir_path = config$work_dir_path
output_dir <- file.path(work_dir_path, "output")
print("Output_dir")
print(output_dir)
create_directory(output_dir, key_name)


library(stringr)
steps <- step_names

for (step in steps) {
    print("Step is")
    print(step)
    step_dir_path <- file.path(output_dir, key_name, step)
    print("Step dir path is")
    print(step_dir_path)
    create_directory(file.path(output_dir, key_name), step)
    #file_name <- file.path(step_dir_path, paste0(str_pad(as.character(steps), 2, pad = "0"), "_main"))
    # Add code here to read the file
}