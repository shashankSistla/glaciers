parse_key <- function(args) {

    if (length(args) == 0) {
        stop("No arguments supplied!", call. = FALSE)
    }
    if (length(args) > 1){
        stop("Too many arguments! Supply only one key", call. = FALSE)
    }

    work_dir_path = config$work_dir_path
    keys_dir_path = paste0(work_dir_path, "input")
    print(keys_dir_path)
    file_list_raw <- list.files(path = keys_dir_path, pattern = "\\.R$")
    file_list <- sub("\\.R$", "", file_list_raw)
    print(file_list)
    if (!args %in% file_list) {
        stop("Key isn't present in inputs", call. = FALSE)
    }

    return(args)
}

create_directory <- function(base_path, new_dir_name) {
  new_dir_path <- file.path(base_path, new_dir_name)
  if (!dir.exists(new_dir_path)) {
    dir.create(new_dir_path)
    cat("Directory created:", new_dir_path, "\n")
  } else {
    cat("Directory already exists:", new_dir_path, "\n")
  }
}



setup_output_directory <- function(key) {
    
}