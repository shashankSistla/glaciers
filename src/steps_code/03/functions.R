landsatRead <- function(filenames){
  library(rgdal)
  library(raster)
  landsatImgs = list()
  k = 1
  
  # Edits 1/18/23 XW: change it to full length because unsure of issue of different DEM
  # for(i in 1:(length(filenames)-2)){ #Change this to -2 for now to deal with the some having a different DEM
  for(i in 1:(length(filenames))){ #Change this to -2 for now to deal with the some having a different DEM
        temp = stack(filenames[i])
      #temp = projectRaster(temp, crs='+proj=longlat +datum=WGS84') 
      landsatImgs[k] = temp
      k = k + 1
    }
  return(list("landsatImgs" = landsatImgs))
}

computeWeights <- function(weighting = 'linear',coord.parallel){
  numparallel <- ncol(coord.parallel)/2
  if (weighting == "linear") {
    weight = c((numparallel+1)/2, rep(((numparallel-1)/2) : 1, each = 2))
  }
  else if(weighting == "central") {
    weight = c(1, rep(0,(numparallel-1)))
  }
  else if(weighting == "equal") {
    weight = rep(1/numparallel, numparallel)
  }
  return(weight)
}

bandsNDSI <- function(landsatReadOutput){
  NDSI = list()
  landsatImgs = landsatReadOutput$landsatImgs
  
  last_img = landsatReadOutput$landsatImgs[[length(landsatImgs)]]
  #for (i in 1:(length(landsatImgs) - 1)){
  for (i in 1:length(landsatReadOutput$landsatImgs)){
    if(dim(landsatReadOutput$landsatImgs[[i]])[3] == 12){
      B5_swir = landsatReadOutput$landsatImgs[[i]][[6]]
      B2_green = landsatReadOutput$landsatImgs[[i]][[3]]
      NDSI[i] = ( B2_green - B5_swir)/(B2_green + B5_swir)
    }else{
      B5_swir = landsatReadOutput$landsatImgs[[i]][[5]]
      B2_green = landsatReadOutput$landsatImgs[[i]][[2]]
      NDSI[i] = (B2_green - B5_swir)/(B2_green + B5_swir)
    }
    
    if(!compareCRS(NDSI[[i]], last_img)){
      proj_img = projectRaster(NDSI[[i]], last_img)
    } else {
      proj_img = NDSI[[i]]
    }
    NDSI[[i]] = proj_img
  }
  
  
  return(NDSI)
}


SinglePathIntensity <- function(band, path,dem){
  library(raster)
  library(velox)
  lengthpath = nrow(path)
  
  intensity_list = rep(0, lengthpath)
  
  xmin = extent(band)@xmin
  xmax = extent(band)@xmax
  ymin = extent(band)@ymin
  ymax = extent(band)@ymax
  
  #path_spat = SpatialPoints(path, proj4string = crs(band))
  #UTM coordinates do not match between all the images. Put all into the zone of the last image, filter
  #within the landsat reading function.
  #library("prioritizr")
  #intensity_list = ifelse(path[,1] >= xmin & path[,1]<= xmax & path[,2]>= ymin & path[,2]<= ymax,fast_extract(band, path_spat), 0)
  #intensity_list = ifelse(path[,1] >= xmin & path[,1]<= xmax & path[,2]>= ymin & path[,2]<= ymax,extract(band, path), 0)
  #intensity_list = raster::extract(band, path)
  
  # velox_raster does not respect the order of the flowline, so need to reverse it
  velox_raster = velox(band)
  # velox_raster = velox(projectRaster(band, crs = crs(dem)))
  
  # Approach 1: use the points as individual, and concatenate them (make sure order is kept)
  # advantage: order is kept
  # disadvantage: more NAs than the spatial lines
  path_fun = function(path_i) {
    return (velox_raster$extract_points(SpatialPoints(t(as.data.frame(path_i)), proj4string=crs(dem))))
  }
  intensity_vector_ind = apply(path, FUN = path_fun, MARGIN = 1)
  
  
  # # Approach 2: use spatial lines along the path, and examine the order to see if it needs to be reversed
  # path_sp = spLines(SpatialPoints(path, proj4string=crs(dem)))
  # intensity_vector = velox_raster$extract(path_sp)[[1]]
  
  # # Approach 3: use original methods
  # intensity_list = ifelse(x >= xmin & x<= xmax & y>= ymin & y<= ymax, extract(band, path), 0)
    
  
  
  # 
  # # check_idx is the first index of intensity_vector that differs looking forward and backward
  # check_idx = min(which((intensity_vector != rev(intensity_vector)
  #                        & !is.na(intensity_vector)
  #                        & !is.na(rev(intensity_vector)))))
  # # this is checking criteria
  # path_sp_init = SpatialPoints(path[check_idx,], proj4string=crs(dem))
  # intensity_vector_init = velox_raster$extract_points(path_sp_init)[[1]]
  # # on the checked point, if the velox function reverses it, flip it
  # if (intensity_vector_init != intensity_vector[check_idx]) {
  #   intensity_vector = rev(intensity_vector)
  # }

  return(intensity_vector_ind)
}

#Helper function to deal with all intensity paths and then compute the weighted 
#Intensity paths as list
#Takes list of intensity profiles created via a loop and then makes a list of intesnities
#compute weighted intensity profile based on the different intensity profiles


getweightedIP <- function(dem, computedWeights, coord.parallel, band){
  numparallel = ncol(coord.parallel)/2
  intensity_list = list(0)
  intensity_length = numeric()
  for(j in 1:numparallel){
    intensity = SinglePathIntensity(band,coord.parallel[,(2*(j)-1):(2*j)], dem)
    intensity_list[[j]] = intensity
    #print(intensity_list)
    intensity_length[j] = length(intensity)
  }
  if(var(intensity_length) == 0){
    intensity_list = do.call(cbind, intensity_list)
  }else{
    length_needed = max(intensity_length)
    indices = which(intensity_length != length_needed)
    for(i in indices){
      num_to_add = length_needed - length(intensity_list[[i]])
      intensity_list[[i]] = append(intensity_list[[i]], rep(0, num_to_add))
    }
     intensity_list = do.call(cbind, intensity_list)
  }
 
  ind = (intensity_list != 0)*1
  weight = computeWeights('linear', coord.parallel)
  #Need to redo these next two lines to make sure that we are computing weighted
  #Intensity by row using the indicator computed above
  if(is.null(intensity_list)){
    print("NAs")
    return()
  }
  print("NOT NAs")
  valuesIntensity = apply(weight*intensity_list,1, sum, na.rm = T)/apply(weight*ind,1,sum, na.rm = T)
  #print(valuesIntensity)
  pmissing = 0.10
  if(sum(is.na(valuesIntensity)) > 0 & sum(is.na(valuesIntensity)) < 0.8*length(valuesIntensity)){
    valuesIntensity = missingInterpolate(valuesIntensity, pmissing)
  }
  return(valuesIntensity)
}

#Helper function to compute weights for taking the average of intensity profiles
#WEighting can be linear, central or equal. As a default we use linear weighting
computeWeights <- function(weighting = 'linear', coord.parallel){
  numparallel <- ncol(coord.parallel)/2
  if (weighting == "linear") {
    weight = c((numparallel+1)/2, rep(((numparallel-1)/2) : 1, each = 2))
  }
  else if(weighting == "central") {
    weight = c(1, rep(0,(numparallel-1)))
  }
  else if(weighting == "equal") {
    weight = rep(1/numparallel, numparallel)
  }
  return(weight)
}


missingInterpolate <- function(valuesIntensity, pmissing){
  library(mgcv)
  y = valuesIntensity
  c = sum(is.na(y))
  length = length(y)
  if(c>0){
    if(c<=pmissing*length){
      y1 = y[which(!is.na(y))]
      x1 = which(!is.na(y))
      x2 = which(is.na(y))
      g = mgcv::gam(y1~s(x1, k = length(x1)/10))
      y[which(is.na(y))] <- predict(g,data.frame(x1=x2))
    }else{y = 0}  # value is 0 if more than 20% is missing on the intensity profile 
    
  }
  value = y 
  return(value)
}


# need to change such that it takes in the pixels but then converts to 
#coordinates so that we can compute intensity profile on landsat even though images
#do not match up in size for some reason

#xFromCol (gives coordinate at x value)
#yFromRow (gives coordinate at y value)
#extract(object, cellxy )[1]

decimaldate <- function(date){
  
  year = as.Date(date)
  year = format(year,"%Y")
  year = as.numeric(as.character(year))
  first = as.Date(paste(year, "-01-01", sep = ''))
  
  diff = as.numeric(as.Date(date) - first)
  
  number_of_days = ifelse(year %% 4 == 0, 366, 365)
  decimal=diff/number_of_days
 
  decimal.date = year + decimal
 
  return(decimal.date)
}