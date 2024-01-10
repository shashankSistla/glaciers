main.function_03_extract_IP <- function(key, root_dir){

  #install.packages("~/velox_0.2.0.tar.gz", repos = NULL, type = "source")
  # STEP NAME
  step_name = "03_extract_IP"

  # LIBRARIES 
  library(devtools)
  library(velox)

  # SOURCE
  source(paste0(root_dir,"/config.R"))
  source(paste0(root_dir, "/output/",key,"/03_extract_IP/glacier_list.R"))
  source(paste0(root_dir, "/src/steps_code/03_extract_IP/functions.R"))
  source(paste0(root_dir, "/src/base_functions.R"))
  source(paste0(root_dir, "/keys/", key,".R"))

  
  # PATH MANAGEMENT
  work_dir_path = config$work_dir_path
  landsat_images_dir_path = config$landsat_images_dir_path
  step_01_output_dir = paste0(root_dir, "/output/", key, "/01_prepare_dem/output/")
  step_02_output_dir = paste0(root_dir, "/output/", key, "/02_GD_flowline/output/")
  output_dir_path = paste0(work_dir_path, "/output/",key,"/",step_name)
  create_directory(output_dir_path, "output")

  # LOAD NECESSARY FILES AND PARAMS
  max_frac_na_in_date = params$step_3$max_frac_na_in_date
  max_frac_na_in_path = params$step_3$max_frac_na_in_path

  glacier_count = 1
  for(glacier in glacier_list){

      #Logging progress
      glacier_count = progress(glacier, glacier_count)

      # Loading parallel path co-ordinates
      coord.parallel = readRDS(paste0(step_02_output_dir, glacier, "_coord_parallel.rds"))

      # Getting landsat image filenames
      glacier_landsat_images_dir = paste0(landsat_images_dir_path, "/", glacier,"/")
      setwd(glacier_landsat_images_dir)
      filenames = list.files(getwd())
      filenames = filenames[grepl(".tif", filenames, fixed = TRUE)]

      #Reading landsat images
      print("Reading landsat images")
      landsatReadOutput = landsatRead(filenames)

      # Compute weights
      weight = computeWeights(weighting = "linear", coord.parallel)

      # NDSI Compute
      NDSI = bandsNDSI(landsatReadOutput)

      # Load DEM
      dem_path = paste0(step_01_output_dir, glacier,"_dem.tif")
      dem = raster(dem_path)

      # can be speedened if CRS is already same
      # Project to CSR of DEM
      print("Projecting NDSI to DEM")
      NDSI_projected <- sapply_with_progress(NDSI, FUN = function(x) {
          projectRaster(x, crs = crs(dem))
          }, simplify = FALSE)

      # Get weighted IP
      print("Computing weighted IP")
      ts_int = sapply_with_progress(NDSI_projected, FUN = getweightedIP, coord.parallel = coord.parallel, computedWeights = weight, dem = dem)

      #This makes a matrix from the list
      if(is.list(ts_int)){
        ts_int=do.call(cbind,ts_int)
      }
    
      #Transposing makes it so that the columns correspond to path points instead of the dates
      # Shape of ts_int is (dates x arclengths)
      ts_int = t(ts_int)
      print("Dim of ts_int")
      print(dim(ts_int))
      ## 2/7/2023 XW comment out below two lines: it seems like the flipping is not needed
      # if(abs(mean(ts_int[,1], na.rm = T)) > abs(mean(ts_int[,ncol(ts_int)], na.rm = T))){
      #   ts_int<-ts_int[,c(ncol(ts_int):1)]
      # }
      
      #Get dates from file names
      dates <- as.Date(unlist(strsplit(filenames, split = "_", fixed = TRUE))[seq(2,5*length(filenames), 5)])

      #Convert dates to a decimal year
      dates = decimaldate(dates)[1:nrow(ts_int)]

      #Remove the columns (path points) in which we have more than 80% NAs
      cut_off_date_index = 0
      for(i in 1:ncol(ts_int)){
        intensities_along_path = ts_int[,i]  # this selects the intensities along a path for a given date
        threshold_path_na = max_frac_na_in_path*nrow(ts_int) # this computes threshold of rows which can be na
        rows_na = sum(is.na(intensities_along_path))
        if(rows_na > threshold_path_na){
          cut_off_date_index = i - 1
          break
        }
      }
      
      if(cut_off_date_index > 0){
        ts_int = ts_int[,1:cut_off_date_index]
      }

      #Get arc length in meters between each path point
      # TODO: Correct the arclength within the function itself
      if(cut_off_date_index > 0){
        al = arclength(coord.parallel[,1:2], dem)[1:cut_off_date_index]
      }else{
        al = arclength(coord.parallel[,1:2], dem)
      }

      #Remove dates that have too much missingness (>50%)
      indices_dates_na = c()
      j = 0
      for(i in 1:nrow(ts_int)){
        intensities_on_date = ts_int[i,]
        threshold_dates_na = ncol(ts_int)*max_frac_na_in_date
        dates_na = sum(is.na(intensities_on_date))

        if(dates_na > threshold_dates_na){
          j = j + 1
          indices_dates_na[j] = i
        }
      }
 
      #Remove Dates that are flat (have low variance) 
      #remove rows with a lot of NA or really low variance
      indices_to_remove = c()
      # for(i in 1:nrow(ts_int)){
      #   if(sum(is.na(ts_int[i,])) >= (ncol(ts_int) - 5) | var(is.finite(ts_int[i,]), na.rm = T) < 1){
      #     indices_to_remove = append(indices_to_remove, i)
      #   }
      # }

      
      #Check if ts_intensity is all 0 for any dates
      indices_all_zero = which(abs(rowSums(ts_int))<0.1)
      
      #setwd(path)
      indices_to_remove = sort(append(indices_all_zero, indices_to_remove))
      # if(!is.null(indices_dates_na)){
      #   indices_to_remove = sort(append(indices_to_remove, indices_dates_na))
      # }
      indices_to_remove = sort(append(indices_to_remove, indices_dates_na))

      
      # Pruning ts_int
      if(!identical(indices_to_remove, integer(0))){
        ts_intensity <- ts_int[-(indices_to_remove),]
        dates_cut = dates[-(indices_to_remove)]
        #edited_landsat <-landsatReadOutput$landsatImgs[-indices_to_remove]
      } else {
        ts_intensity <- ts_int
        dates_cut = dates
        #edited_landsat <- landsatReadOutput$landsatImgs
      }

      means = rowMeans(ts_intensity)
      Q1 = quantile(means, 0.25)
      Q3 = quantile(means, 0.75)

      IQR32 = 1.5*IQR(ts_intensity)
      outliers = which(means > Q3 + IQR32 | means < Q1 - IQR32)

      if(length(outliers) != 0){
        ts_intensity <- ts_intensity[-(outliers),]
        dates_cut = dates_cut[-(outliers)]
        #edited_landsat <-edited_landsat$landsatImgs[-outliers] # used for save GIF. idea, save just the removed indices and re-create later
      }

      # write.csv(indices_to_remove, paste0(path,"Processed/",glacier, "_filteredimages.csv"))
      # write.csv(outliers, paste0(path,"Processed/",glacier, "_outlierimages.csv"))
      if(length(dates_cut) == 0){
        print("No good intensity profiles")
        next
        }

      dates_cut_filename = paste0(output_dir_path,"/output/", glacier,"_dates_cut.rds")
      saveRDS(dates_cut, file = dates_cut_filename)

      ts_intensity_filename = paste0(output_dir_path, "/output/", glacier,"_ts_intensity.rds")
      saveRDS(ts_intensity, file = ts_intensity_filename)

      al_filename = paste0(output_dir_path, "/output/", glacier,"_al.rds")
      saveRDS(al, file = al_filename)

  }
}