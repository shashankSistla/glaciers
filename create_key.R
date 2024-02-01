# Run the script with command line arguments
args <- commandArgs(trailingOnly = TRUE)

library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/src/base_functions.R"))
source(paste0(root_dir, "/src/constants.R"))

key = parse_key(args)

work_dir_path = config$work_dir_path
output_dir <- file.path(work_dir_path, "output")
print("Output_dir")
print(output_dir)
create_directory(output_dir, key)
key_identifier = "\\$KEY"
step_identifier = "\\$xx"

library(stringr)
steps <- step_names

for (step in steps) {
    step_dir_path <- file.path(output_dir, key, step)
    create_directory(file.path(output_dir, key), step)

    input_script_path <- paste0(root_dir, "/src/main_template.R")
    output_script_path <- paste0(root_dir, "/output/", key, "/", step, "/main.R")

    script_content <- readLines(input_script_path)
    modified_script_content <- str_replace_all(script_content, key_identifier, key)
    modified_script_content <- str_replace_all(modified_script_content, step_identifier, step)
    writeLines(modified_script_content, output_script_path)

    input_script_path <- paste0(root_dir, "/glacier_list_main.R")
    output_script_path <- paste0(root_dir, "/output/", key,"/", step, "/glacier_list.R")
    script_content <- readLines(input_script_path)
    writeLines(script_content, output_script_path)
    output_script_path <- paste0(root_dir, "/output/", key, "/key_glacier_list.R")
    writeLines(script_content, output_script_path)

    input_script_path <- paste0(root_dir, "/keys/00key_template.R")
    output_script_path <- paste0(root_dir, "/keys/", key,".R")
    script_content <- readLines(input_script_path)
    writeLines(script_content, output_script_path)

    input_script_path <- paste0(root_dir, "/src/run_scripts_template.R")
    output_script_path <- paste0(root_dir, "/output/", key, "/run_all_steps.R")
    script_content <- readLines(input_script_path)
    modified_script_content <- str_replace_all(script_content, key_identifier, key)
    writeLines(modified_script_content, output_script_path)
}