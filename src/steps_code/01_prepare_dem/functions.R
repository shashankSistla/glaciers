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