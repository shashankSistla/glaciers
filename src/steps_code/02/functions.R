GD.linear.sample <- function(dem, initial.coord,step.size = 5, sample = 0, diag = 500, option ="median", block= 30, blocks = c(30,40,50,60)){
  #demmatrix <- as.matrix(demmatrix)
  
  #nrow <- nrow(demmatrix)
  #ncol <- ncol(demmatrix)
  x_low <- extent(dem)[1]
  x_high <- extent(dem)[2]
  y_low <- extent(dem)[3]
  y_high <- extent(dem)[4]
  
  if(sample == 0){ 
    k = 1 
  }else{
    k = ceiling(norm(dim(dem),type="2")/diag)
    #k = 1
  }
  #Shouldn't need these two lines with the new UTM format
  #demmatrix2 = demmatrix[seq(1:ceiling(nrow/k))*k-(k-1),seq(1:ceiling(ncol/k))*k-(k-1)]
  #coord = ceiling(initial.coord/k)
  coord = initial.coord
  #step.size = ceiling(5/k)
  if(option == "regular"){
    result = GD(dem, coord, block, step.size, nrow, ncol)
    coord.matrix = result$coord
    complete = result$complete
    warning = table(result$warning)
  }
  
  if(option == "median"){
    coord.unsmooth = coord.smooth = coord.ele = warning = NULL
    complete.list = c()
    for(j in 1:length(blocks)){
      print(j)
      path = GD(dem, coord, blocks[j], step.size = step.size, nrow, ncol)
      #print("made through gd")
      coord.unsmooth[[j]] = path$coord
      #print(path$coord)
      coord.smooth[[j]] = path_smooth(path$coord, step.size = step.size*30, thin = 3)
      print("path smooth")
      complete.list = c(complete.list, path$complete)
      warning = c(warning, path$warning)
    }
    coordx = coordy = matrix(NA,nrow = max(unlist(lapply(coord.smooth, nrow))),ncol = length(blocks))
    for(j in 1:length(blocks)){
      coordx[1:nrow(coord.smooth[[j]]), j] = coord.smooth[[j]][,1]
      
      if(nrow(coord.smooth[[j]]) < nrow(coordx)){
        coordx[(nrow(coord.smooth[[j]])+1):nrow(coordx),j] = coordx[nrow(coord.smooth[[j]]),j]
      }
      coordy[1:nrow(coord.smooth[[j]]), j] = coord.smooth[[j]][,2]
      if(length(coord.smooth[[j]][,2]) < nrow(coordy)){
        coordy[(nrow(coord.smooth[[j]])+1):nrow(coordy),j] = coordy[nrow(coord.smooth[[j]]),j]
      }
    }
    #If there is only one that has one that length shouldnt we just take that one
    coord.matrix = matrix(c(rowMedians(coordx, na.rm = T), rowMedians(coordy, na.rm = T)), ncol = 2)
    same = match(coord.matrix[nrow(coord.matrix),], coord.matrix)[1]
    coord.matrix = coord.matrix[1:same,]
    coord.matrix = path_smooth(coord.matrix, step.size = step.size*30, thin = 3)
    complete = max(complete.list)
    warning = table(warning)/length(blocks)
  } 
  coord = k*coord.matrix
  return(list(coord = coord, complete = complete, warning = warning))
}

GD <- function(dem, initial.coord, block, step.size, nrow, ncol){
  #matrix2 = as.matrix(matrix2)
  complete = 0
  block_dist = block*30 
  b = block_dist/2
  #nrow2 <- nrow(matrix2)
  x_low = extent(dem)[1] 
  x_high = extent(dem)[2]
  y_low = extent(dem)[3]
  y_high = extent(dem)[4]
  step.size = step.size*30
  #ncol2 <- ncol(matrix2)
  maxiters <- round((x_high - x_low + y_high - y_low) / step.size)
  print(maxiters)
  coord.matrix <- matrix(initial.coord, nrow = 1)
  coord = initial.coord
  warning <- c()
  for(i in 1:maxiters){
    #print(i)
    x <- coord[1]
    y <- coord[2]
    xmin <- max(round(x-b), x_low)
    xmax <- min(round(x+b), x_high)
    ymin <- max(round(y-b),y_low)
    ymax <- min(round(y+b), y_high)
    e <- as(extent(xmin,xmax,ymin,ymax), "SpatialPolygons")
    crs(e) <- crs(dem)
    dem_crop <- crop(dem, e)
    if(base::sum(is.na(as.matrix(dem_crop))) > (0.5*dim(dem_crop)[1]*dim(dem_crop)[2])){
      next
    }
    
    #nrow1 = nrow(block)
    #ncol1 = ncol(block)
    nrow1 <- ncol(dem_crop) # When in a raster Y respresents row (why I don't know?)
    ncol1 <- nrow(dem_crop)
    Z<- as.vector(dem_crop) 
    X <- as.numeric(gl(nrow1,1,nrow1*ncol1))
    Y= as.numeric(gl(ncol1,nrow1,ncol1*nrow1))
    lm = lm(Z ~ X+Y ) 
    
    
    dx= summary(lm)[4][[1]][2,1]
    dy= summary(lm)[4][[1]][3,1]
    if(dx ==0 | dy ==0 ){break}
    multiplier <- step.size/ sqrt(dx^2 + dy^2)
    x <- x - multiplier * dx
    y <- y + multiplier * dy
    coord <- c(x,y)
    new.coord <- matrix(coord, nrow=1) # confirmed it moves 15 meters
    if(x<=x_low|x>=x_high|y<=y_low|y>=y_high){
      if(i == 1) {
        warning <- c(warning, "initial_point_outside_of_DEM")
      } else {
        warning <- c(warning, "Middle_points_outside_of_DEM")
      }
      # if points exceed the edge of the frame, use the edge of the frame
      if (x <= x_low){x = x_low}
      if (x >= x_high){x = x_high}
      if (y <= y_low){y = y_low}
      if (y >= y_high){y = y_high}
      complete = 1 
    break} # terminate at boundary
    if(nrow(merge(coord.matrix,new.coord))>0){break} # terminate at loop
    coord.matrix <- rbind(coord.matrix, new.coord)  
  }
  coord = coord.matrix
  #   print(coord)
  coord= coord[which(coord[,1] <= x_high & coord[,1] >= x_low & coord[,2] <= y_high & coord[,2] >= y_low),]
  #print(coord)
  #   print(coord)
  list(coord =coord, complete = complete, warning = warning)
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