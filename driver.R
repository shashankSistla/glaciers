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
key_identifier = "\\$KEY"
step_number_identifier = "\\$xx"

library(stringr)
steps <- step_names

step_idx = 0
for (step in steps) {
    step_idx = step_idx + 1
    step_dir_path <- file.path(output_dir, key_name, step)
    create_directory(file.path(output_dir, key_name), step)

    input_script_path <- paste0(root_dir, "/src/main_template.R")
    output_script_path <- paste0(root_dir, "/output/", key_name, "/", step, "/main.R")

    script_content <- readLines(input_script_path)
    modified_script_content <- str_replace_all(script_content, key_identifier, key_name)
    modified_script_content <- str_replace_all(modified_script_content, step_number_identifier, sprintf("%02d", step_idx))
    writeLines(modified_script_content, output_script_path)

    input_script_path <- paste0(root_dir, "/glacier_list_main.R")
    output_script_path <- paste0(root_dir, "/output/", key_name,"/", step, "/glacier_list.R")
    script_content <- readLines(input_script_path)
    writeLines(script_content, output_script_path)

    input_script_path <- paste0(root_dir, "/input/00input_template.R")
    output_script_path <- paste0(root_dir, "/input/", key_name,".R")
    script_content <- readLines(input_script_path)
    writeLines(script_content, output_script_path)
    #print(file_name)
    # Add code here to read the file
    #TODO add glacier_list to config

}