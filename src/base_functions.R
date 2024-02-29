parse_key <- function(args) {

    if (length(args) == 0) {
        stop("No arguments supplied!", call. = FALSE)
    }
    if (length(args) > 1){
        stop("Too many arguments! Supply only one key", call. = FALSE)
    }

    work_dir_path = config$work_dir_path
    keys_dir_path = paste0(work_dir_path, "keys")
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

lapply_with_progress <- function(X, FUN, ..., USE.NAMES = TRUE) {
  pb <- txtProgressBar(min = 0, max = length(X), style = 3)
  results <- vector("list", length(X))
  
  for(i in seq_along(X)) {
    results[[i]] <- FUN(X[[i]], ...)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Apply names to the results if USE.NAMES is TRUE and names are available
  if(USE.NAMES && !is.null(names(X))) {
    names(results) <- names(X)
  }

  return(results)
}



progress <- function(glacier, glacier_count){
    print(paste0("Currently processing ",glacier,". ",glacier_count," out of ", length(glacier_list)))
    return(glacier_count + 1)
}

convertDecimalYearToDate <- function(decimalYear) {
  # Extract the year part and calculate the remaining decimal part
  year <- floor(decimalYear)
  decimalPart <- decimalYear - year
  
  # Check if the year is a leap year
  isLeapYear <- ifelse((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, TRUE, FALSE)
  
  # Calculate the day of the year
  dayOfYear <- round(decimalPart * ifelse(isLeapYear, 366, 365))
  
  # Convert the day of the year to a date
  date <- as.Date(paste(year, "-01-01", sep="")) + (dayOfYear - 1)
  
  # Format the date as YYYY-MM-DD
  formattedDate <- format(date, "%Y-%m-%d")
  
  return(formattedDate)
}

landsatRead <- function(filenames){
  library(raster)
  landsatImgs = list()
  k = 1
  
  pb <- txtProgressBar(min = 0, max = length(filenames), style = 3)
  # Edits 1/18/23 XW: change it to full length because unsure of issue of different DEM
  # for(i in 1:(length(filenames)-2)){ #Change this to -2 for now to deal with the some having a different DEM
  for(i in 1:(length(filenames))){ #Change this to -2 for now to deal with the some having a different DEM
        temp = stack(filenames[i])
      #temp = projectRaster(temp, crs='+proj=longlat +datum=WGS84') 
      landsatImgs[k] = temp
      k = k + 1

      setTxtProgressBar(pb, i)
    }
  close(pb)
  return(list("landsatImgs" = landsatImgs))
}