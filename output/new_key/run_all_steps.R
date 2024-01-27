key = "new_key"

library(rprojroot)

root_criterion <- has_file("Glaciers.Rproj")
root_dir <- find_root(root_criterion)

source(paste0(root_dir, "/src/constants.R"))


key_dir = paste0(root_dir, "/output/",key)

input_script_path <- paste0(key_dir, "/key_glacier_list.R")
script_content <- readLines(input_script_path)

  for (step_name in step_names) {
    print(paste("Now running step ", step_name))
    output_script_path <- paste0(root_dir, "/output/", key, "/", step_name, "/glacier_list.R")
    writeLines(script_content, output_script_path)
    main_script_path <- paste0(key_dir, "/",step_name,"/main.R")
    if (file.exists(main_script_path)) {
      source(main_script_path)
      cat("Successfully ran main.R in:", step_name, "\n")
    } else {
      cat("main.R not found in:", step_name, "\n")
    }
    setwd(root_dir)
  }
