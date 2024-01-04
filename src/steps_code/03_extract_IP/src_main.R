main.function_03 <- function(key, root_dir){

#install.packages("~/velox_0.2.0.tar.gz", repos = NULL, type = "source")
step_name = "03_extract_IP"

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/output/",key,"/03_process_landsat/glacier_list.R"))
source(paste0(root_dir, "/src/steps_code/03/functions.R"))
source(paste0(root_dir, "/src/base_functions.R"))
source(paste0(root_dir, "/keys/", key,".R"))

library(devtools)
library(velox)

work_dir_path = config$work_dir_path
landsat_images_dir_path = config$landsat_images_dir_path


step_01_output_dir = paste0(root_dir, "/output/", key, "/01_prepare_dem/output/")
step_02_output_dir = paste0(root_dir, "/output/", key, "/02_GD_flowline/output/")

max_percent_missing = params$step_3$max_percent_missing
max_percent_na = params$step_3$max_percent_na

output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
create_directory(output_dir, "output")


for(glacier in glacier_list){
    coord.parallel = readRDS(paste0(step_02_output_dir, glacier, "_coord_parallel.rds"))
    glacier_landsat_images_dir = paste0(landsat_images_dir_path, "/", glacier,"/")
    setwd(glacier_landsat_images_dir)
    filenames = list.files(getwd())
    filenames = filenames[grepl(".tif", filenames, fixed = TRUE)]
    #print(filenames)
    landsatReadOutput = landsatRead(filenames)

    # Add this to config
    numparallel = 3
    weight = computeWeights(weighting = "linear", coord.parallel)
    NDSI = bandsNDSI(landsatReadOutput)

    dem_path = paste0(step_01_output_dir, glacier,"_dem.tif")
    dem = raster(dem_path)

    for (i in 1:length(NDSI)){
      NDSI[[i]] <- projectRaster(NDSI[[i]], crs = crs(dem))
    }

    ts_int = list()
    ts_int = sapply(NDSI, FUN = getweightedIP, coord.parallel = coord.parallel, computedWeights = weight, dem = dem)
        
    #This makes a matrix from the list
    if(is.list(ts_int)){
      ts_int=do.call(cbind,ts_int)
    }
   
    #Transposing makes it so that the columns correspond to path points instead of the dates
    ts_int = t(ts_int)
    forNDSI = T
    
    ## 2/7/2023 XW comment out below two lines: it seems like the flipping is not needed
    # if(abs(mean(ts_int[,1], na.rm = T)) > abs(mean(ts_int[,ncol(ts_int)], na.rm = T))){
    #   ts_int<-ts_int[,c(ncol(ts_int):1)]
    # }
    
    #Get dates from file names
    # dates = as.Date(substr(filenames[-length(filenames)],16,nchar(filenames)-14))
    # dates_full = as.Date(substr(filenames[-length(filenames)],16,nchar(filenames)-14))[1:length(NDSI)]
    dates <- as.Date(unlist(strsplit(filenames, split = "_", fixed = TRUE))[seq(2,5*length(filenames), 5)])
    #setwd(path)

    #convert dates to a decimal year
    dates = decimaldate(dates)[1:nrow(ts_int)]

    #Remove the columns (path points) in which we have more than 80% NAs
    index = 0
    for(i in 1:ncol(ts_int)){
      if(sum(is.na(ts_int[,i])) > (.80)*nrow(ts_int)){
        index = i - 1
        #print(index)
        break
      }
    }
    
    if(index > 0){
      ts_int = ts_int[,1:index]
    }

    #Remove dates that have too much missingness (>50%)
    pmissing = max_percent_missing
    indices = c()
    j = 0
    for(i in 1:nrow(ts_int)){
      if(sum(is.na(ts_int[i,]))/ncol(ts_int) > pmissing){
        j = j + 1
        indices[j] = i
      }
    }

    #Get arc length in meters between each path point
    if(index > 0){
      al = arclength(coord.parallel[,1:2], dem)[1:index]
    }else{
      al = arclength(coord.parallel[,1:2], dem)
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
    if(!is.null(indices)){
      indices_to_remove = sort(append(indices_to_remove, indices))
    }

    #Indices to remove is now all the indices we are getting rid of for the glacier and we remove them from ts_int
    if(!identical(indices_to_remove, integer(0))){
      ts_intensity <- ts_int[-(indices_to_remove),]
      dates_cut = dates[-(indices_to_remove)]
      edited_landsat <-landsatReadOutput$landsatImgs[-indices_to_remove]
    } else {
      ts_intensity <- ts_int
      dates_cut = dates
      edited_landsat <- landsatReadOutput$landsatImgs
    }
    means = rowMeans(ts_intensity)
    Q3 = quantile(means, 0.75)
    Q1 = quantile(means, 0.25)
    IQR32 = 1.5*IQR(ts_intensity)
    outliers = which(means > Q3 + IQR32 | means < Q1 - IQR32)
    if(length(outliers) != 0){
      ts_intensity <- ts_intensity[-(outliers),]
      dates_cut = dates_cut[-(outliers)]
      edited_landsat <-edited_landsat$landsatImgs[-outliers]
    }

    # write.csv(indices_to_remove, paste0(path,"Processed/",glacier, "_filteredimages.csv"))
    # write.csv(outliers, paste0(path,"Processed/",glacier, "_outlierimages.csv"))
    if(length(dates_cut) == 0){
      print("No good intensity profiles")
      next
      }

    dates_cut_filename = paste0(output_dir,"/output/", glacier,"_dates_cut.rds")
    saveRDS(dates_cut, file = dates_cut_filename)

    ts_intensity_filename = paste0(output_dir, "/output/", glacier,"_ts_intensity.rds")
    saveRDS(ts_intensity, file = ts_intensity_filename)

    al_filename = paste0(output_dir, "/output/", glacier,"_al.rds")
    saveRDS(al, file = al_filename)

}
}