    calculate_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    get_joined_file <- function(x) {
      work_dir_path = config$work_dir_path
      joined_csv_path = paste0(work_dir_path,"/bin/joined.csv")
      glaciers <- read.csv(joined_csv_path, header = T)
      return(glaciers)
    }

    extract_image_extent <- function (image) {
      print(image)
      return (as.list(extent(stack(image))))
    }

    readDEM <- function(demfilename){
      library(raster)
      library(rgdal)
      #f <- paste(newpath, "/", demfilename, sep = "")
      imported_raster=raster(demfilename)
      raster::as.matrix(imported_raster)
      demmatrix <- imported_raster
      #Do not need to rotate
      #if(all(as.matrix(demmatrix) == 0)){
      #print("DEM is all zeros") 
      #}
      return(demmatrix)
    }


    initial_to_UTM <- function(x,y, crs_of_landsat){
    cbind(x,y)
    coord = as.data.frame(cbind(x, y), nrow = 1)
    names(coord) = c("long", "lat")
    cord.dec = SpatialPoints(cbind(coord$long, coord$lat), proj4string=CRS("+proj=longlat"))
    
    cord.UTM <- spTransform(cord.dec, crs_of_landsat)
    initial.coord=as.data.frame(cord.UTM) 
    
    #initial.coord = abs(initial.coord)
    initial.coord = as.vector(t(initial.coord))
    return(initial.coord)
    }