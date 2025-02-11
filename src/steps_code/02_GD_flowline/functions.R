GD.linear.sample <- function(dem, initial.coord, step.size = 5, sample = 0, diag = 500, option = "median", blocks = c(30, 40, 50, 60), output_dir, glacier) {
  x_low <- extent(dem)[1]
  x_high <- extent(dem)[2]
  y_low <- extent(dem)[3]
  y_high <- extent(dem)[4]
  
  if (sample == 0) { 
    k = 1 
  } else {
    k = ceiling(norm(dim(dem), type = "2") / diag)
  }
  
  coord = initial.coord
  frames_dir = paste0(output_dir, "frames")
  coord_unsmooth = coord_smooth = coord_ele = warning = NULL
  complete_list = c()
  for (j in 1:length(blocks)) {
    path = GD(dem, coord, blocks[j], step.size = step.size, nrow, ncol, frames_dir, glacier, j)
    coord_unsmooth[[j]] = path$coord
    coord_smooth[[j]] = path_smooth(path$coord, step.size = step.size * 30, thin = 3)
    complete_list = c(complete_list, path$complete)
    warning = c(warning, path$warning)
  }
  
  coordx = coordy = matrix(NA, nrow = max(unlist(lapply(coord_smooth, nrow))), ncol = length(blocks))
  for (j in 1:length(blocks)) {
    coordx[1:nrow(coord_smooth[[j]]), j] = coord_smooth[[j]][,1]
    if (nrow(coord_smooth[[j]]) < nrow(coordx)) {
      coordx[(nrow(coord_smooth[[j]]) + 1):nrow(coordx), j] = coordx[nrow(coord_smooth[[j]]), j]
    }
    coordy[1:nrow(coord_smooth[[j]]), j] = coord_smooth[[j]][,2]
    if (length(coord_smooth[[j]][,2]) < nrow(coordy)) {
      coordy[(nrow(coord_smooth[[j]]) + 1):nrow(coordy), j] = coordy[nrow(coord_smooth[[j]]), j]
    }
  }
  
  coord_matrix = matrix(c(rowMedians(coordx, na.rm = TRUE), rowMedians(coordy, na.rm = TRUE)), ncol = 2)
  same = match(coord_matrix[nrow(coord_matrix),], coord_matrix)[1]
  coord_matrix = coord_matrix[1:same,]
  coord_matrix = path_smooth(coord_matrix, step.size = step.size * 30, thin = 3)
  complete = max(complete_list)
  warning = table(warning) / length(blocks)
  
  coord = k * coord_matrix
  
  # Save final frame showing all paths and median path
  final_frame_path <- file.path(frames_dir, "final_frame.png")
  print("glacier")
  print(glacier)
  print("coord_unsmooth")
  print(coord_unsmooth)
  plot_all_paths(dem, coord_unsmooth, coord_matrix, final_frame_path)
  
  gif_path <- file.path(output_dir, paste0(glacier, "_flowline_animation.gif"))
  frames <- list.files(frames_dir, pattern = "*.png", full.names = TRUE)
  images <- image_read(frames)
  
  # Extend the final frame duration
  final_image <- image_read(final_frame_path)
  images <- c(images, rep(final_image, 5))
  
  image_write(image_animate(images, fps = 2), gif_path)
  file.remove(list.files(frames_dir, full.names = TRUE))
  
  return(list(coord = coord, complete = complete, warning = warning))
}

GD <- function(dem, initial.coord, block, step.size, nrow, ncol, frames_dir, glacier, window_index) {
  complete = 0
  block_dist = block * 30 
  b = block_dist / 2
  x_low = extent(dem)[1] 
  x_high = extent(dem)[2]
  y_low = extent(dem)[3]
  y_high = extent(dem)[4]
  step.size = step.size * 30
  maxiters <- round((x_high - x_low + y_high - y_low) / step.size)
  coord.matrix <- matrix(initial.coord, nrow = 1)
  coord = initial.coord
  warning <- c()
  
  for (i in 1:maxiters) {
    x <- coord[1]
    y <- coord[2]
    xmin <- max(round(x - b), x_low)
    xmax <- min(round(x + b), x_high)
    ymin <- max(round(y - b), y_low)
    ymax <- min(round(y + b), y_high)
    e <- as(extent(xmin, xmax, ymin, ymax), "SpatialPolygons")
    crs(e) <- crs(dem)
    dem_crop <- crop(dem, e)
    if (base::sum(is.na(as.matrix(dem_crop))) > (0.5 * dim(dem_crop)[1] * dim(dem_crop)[2])) {
      next
    }
    
    nrow1 <- ncol(dem_crop)
    ncol1 <- nrow(dem_crop)
    Z <- as.vector(dem_crop) 
    X <- as.numeric(gl(nrow1, 1, nrow1 * ncol1))
    Y <- as.numeric(gl(ncol1, nrow1, ncol1 * nrow1))
    lm <- lm(Z ~ X + Y ) 
    
    dx <- summary(lm)[4][[1]][2,1]
    dy <- summary(lm)[4][[1]][3,1]
    if (dx == 0 | dy == 0) { break }
    multiplier <- step.size / sqrt(dx^2 + dy^2)
    x <- x - multiplier * dx
    y <- y + multiplier * dy
    coord <- c(x, y)
    new.coord <- matrix(coord, nrow = 1)
    if (x <= x_low | x >= x_high | y <= y_low | y >= y_high) {
      if (i == 1) {
        warning <- c(warning, "initial_point_outside_of_DEM")
      } else {
        warning <- c(warning, "Middle_points_outside_of_DEM")
      }
      if (x <= x_low) { x = x_low }
      if (x >= x_high) { x = x_high }
      if (y <= y_low) { y = y_low }
      if (y >= y_high) { y = y_high }
      complete = 1 
      break
    } 
    if (nrow(merge(coord.matrix, new.coord)) > 0) { break }
    coord.matrix <- rbind(coord.matrix, new.coord)
    
    # Save current frame with window size label
    frame_path <- file.path(frames_dir, sprintf("window_%02d_frame_%03d.png", window_index, i))
    plot_dem_frame(dem, coord.matrix, new.coord, frame_path, block)
  }
  
  coord = coord.matrix
  coord = coord[which(coord[,1] <= x_high & coord[,1] >= x_low & coord[,2] <= y_high & coord[,2] >= y_low),]
  list(coord = coord, complete = complete, warning = warning)
}


path_parallel <- function(coord, demMatrix, dist = 2.5, numparallel = 1){
  m = nrow(coord)
  dcoord = rbind((coord[2:m,]- coord[1:(m-1),]), (coord[m,] - coord[(m-1),]))
  dcoord = dist*(dcoord/sqrt(dcoord[,1]^2 +dcoord[,2]^2))
  s1 = cbind(dcoord[,2],-dcoord[,1])
  s2 = cbind(-dcoord[,2],dcoord[,1])
  
  
  # delete points out of the image margin 
  df = data.frame(x = coord[,1], y = coord[,2])
  if (numparallel !=0) {
    for ( i in 1:numparallel) {
      coord.parallel = coord + i*s1
      df[paste("x",2*i -1,sep="")] = coord.parallel[,1]
      df[paste("y",2*i -1,sep="")] = coord.parallel[,2]
      
      coord.parallel = coord + i*s2
      df[paste("x",2*i,sep="")] = coord.parallel[,1]
      df[paste("y",2*i,sep="")] = coord.parallel[,2]
    }
  }
  return(df)
}

plot_all_paths <- function(dem, coord_unsmooth, median_coord, frame_path) {
  col_B61 <- colorRampPalette(c("tan4", "lightblue2"))
  col <- col_B61

  png(frame_path)
  image(dem, col = col_B61(20))
  print("This ran")
 # print(coord_unsmooth)
  for (path in coord_unsmooth) {
    lines(path[,1], path[,2], col = "blue")
  }
  print("This also ran")
  lines(median_coord[,1], median_coord[,2], col = "red", lwd = 2)
  dev.off()
}
plot_dem_frame <- function(dem, coord.matrix, current_point, frame_path, window_size) {
  col_B61 <- colorRampPalette(c("tan4", "lightblue2"))
  col <- col_B61
  png(frame_path)
  image(dem, col = col_B61(20))
  lines(coord.matrix[,1], coord.matrix[,2], col = "red")
  points(current_point[1], current_point[2], cex = 1, col = "blue", pch = 16)
  title(main = paste("Window Size:", window_size))
  dev.off()
}



path_smooth <- function(coord, step.size = 150, thin = 3){
  require(mgcv)
  ## 10/9/2023 Update: First check if there is an arc
  if (length(coord) == 2) {
    return (cbind(coord[1], coord[2]))
  }
  arc = c(0,sqrt(rowSums((coord[2:nrow(coord),]-coord[1:(nrow(coord)-1),])^2)))
  cumarc = sapply(1:length(arc), function(x) sum(arc[1:x]))
  #print(cumarc)
  
  fit_arc = seq(1,cumarc[length(cumarc)], step.size/thin)
  
  
  fit_x = matrix(predict(mgcv::gam(c(coord[,1])~s(cumarc,k = round(length(cumarc)/thin))), data.frame(cumarc=fit_arc)), ncol = 1)
  
  fit_y = matrix(predict(mgcv::gam(c(coord[,2])~s(cumarc,k = round(length(cumarc)/thin))), data.frame(cumarc=fit_arc)), ncol = 1) 
  
  
  
  # This next lines repeat the smoothing (possibly could be sloppy previous work)
  #arc = c(0,sqrt(rowSums((coord[2:nrow(coord),]-coord[1:nrow(coord)-1,])^2)))
  
  #cumarc= sapply(1:length(arc), function(x) sum(arc[1:x]))
  
  #fit_arc = seq(1,cumarc[length(cumarc)], step.size)
  #fit_x = matrix(predict(mgcv::gam(c(coord[,1])~s(cumarc,k =round(length(cumarc)/thin))), data.frame(cumarc=fit_arc)), ncol = 1)
  #fit_y = matrix(predict(mgcv::gam(c(coord[,2])~s(cumarc,k = round(length(cumarc)/thin))), data.frame(cumarc=fit_arc)), ncol = 1) 
  return(cbind(fit_x,fit_y))
}


    get_joined_file <- function(x) {
      work_dir_path = config$work_dir_path
      joined_csv_path = paste0(work_dir_path,"/src/joined.csv")
      glaciers <- read.csv(joined_csv_path, header = T)
      return(glaciers)
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

plot_dem <- function(glacier, dem, initial.coord, coord.parallel, plot_path){
  col_B61 <- colorRampPalette(c("tan4", "lightblue2"))
  col <- col_B61
  png(plot_path)
  image(dem, col = col_B61(20))
  lines(coord.parallel$x, coord.parallel$y,col = "red")
  points(initial.coord[1], initial.coord[2], cex = 0.7,col = "black", pch = 16)
  dev.off()
}


