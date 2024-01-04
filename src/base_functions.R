parse_key <- function(args) {

    if (length(args) == 0) {
        stop("No arguments supplied!", call. = FALSE)
    }
    if (length(args) > 1){
        stop("Too many arguments! Supply only one key", call. = FALSE)
    }

    work_dir_path = config$work_dir_path
    keys_dir_path = paste0(work_dir_path, "input")
    file_list_raw <- list.files(path = keys_dir_path, pattern = "\\.R$")
    file_list <- sub("\\.R$", "", file_list_raw)
    if (!args %in% file_list) {
        stop("Key isn't present in keys directory. Create one to get started", call. = FALSE)
    }
    # TODO, check if all inputs are present in key

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

sapply_with_progress <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {

  pb <- txtProgressBar(min = 0, max = length(X), style = 3)
  results <- vector("list", length(X))
  
  for(i in seq_along(X)) {
    results[[i]] <- FUN(X[[i]], ...)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  if(simplify && all(sapply(results, is.atomic))) {
    results <- simplify2array(results, higher = FALSE)
  }

    if(USE.NAMES && is.character(X) && is.null(names(results))){
	    colnames(results) <- X
    }
  return(results)
}


progress <- function(glacier_count){
    print(paste0("Currently processing ",glacier,". ",glacier_count," out of ", length(glacier_list)))
    return(glacier_count + 1)
}