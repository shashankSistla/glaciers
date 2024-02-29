    calculate_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    extract_image_extent <- function (image) {
      return (as.list(extent(stack(image))))
    }

    readDEM <- function(demfilename){
      library(raster)
      imported_raster=raster(demfilename)
      raster::as.matrix(imported_raster)
      demmatrix <- imported_raster
      return(demmatrix)
    }

    # Function to download files from a specific directory in an AWS S3 bucket
download_files_from_s3 <- function(glacier, local_directory) {
  # Load necessary libraries
  library(aws.s3)
  library(pbapply)

  
  # Ensure the directory path ends with a slash
  if (!grepl("/$", s3_directory)) {
    s3_directory <- paste0(s3_directory, "/")
  }
  if (!grepl("/$", local_directory)) {
    local_directory <- paste0(local_directory, "/")
  }
  
  # List files in the directory
  files_in_directory <- get_bucket(bucket = bucket_name, prefix = s3_directory)
  #print(str(files_in_directory))
  
  # Define a function to download a single file
  download_file <- function(file) {
    file_name <- basename(file$Key)
    save_path <- paste0(local_directory, file_name)
    # Correct approach to download and save a file
    save_object(object = file$Key, bucket = bucket_name, file = save_path)

    #s3read_using(FUN = save_object, object = file$Key, bucket = bucket_name, file = save_path)
    return(paste("Downloaded:", file_name))
  }
  
  # Download files with a progress bar
  #results <- pblapply(files_in_directory$Contents, download_file)
  results <- lapply_with_progress(files_in_directory, download_file)
  
  return(results)
}