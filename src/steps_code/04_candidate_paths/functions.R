get_FV_joined_file <- function() {
    work_dir_path = config$work_dir_path
    joined_csv_path = paste0(work_dir_path,"/src/FV_joined.csv")
    FV <- read.csv(joined_csv_path, header = T)
    return(FV)
}

terminus <- function(glacier, obs, ss, tt, meas= NULL, plot = FALSE, direc = NULL, linefit = 1, temporal = 1,invert = 1, distPerYear, knotbuffer = 1, n_paths = 10){
  # tSmooth <- temporal_smooth_mat(obs, tt, knotT = min(round(length(tt)/4)+4, 35+4))$est
  knotbuffer = min(4, knotbuffer) # this is set to be 4 at maximum because the knots may be too little when setting spacing too large
  tSmooth <- temporal_smooth_mat(obs, tt, knotT = round(diff(range(tt))/knotbuffer))$est
  # tSmooth <- obs
  # sSmooth = spatial_smooth(tSmooth, ss, knotS = min(round(diff(range(ss))/3 0), 39))
  sSmooth = spatial_smooth(tSmooth, ss, knotS = min(round(length(ss)/4)+4, 35+4))
  
  
#-------------------------------------------------------------------------------
  #source("~/Glaciers 2/DataForShashank/terminus_est.R")
  term_path = terminus_paths(sSmooth$dd1,tt,ss,glacier,invert=invert,distPerYear, n_paths = n_paths)
#----------------------------------------------------------------------------
  outs <- list()
  if (temporal == 1) {
    # out1 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[1]], knotsT =round(length(tt)/30),meas
  for(i in 1:(2*n_paths)){
    outs[[paste0("out", i)]] <- temporal_smooth(ss, tt, est = sSmooth$est, dd3 = sSmooth$dd3, term_path = term_path[[i]], knotsT = round(diff(range(tt))/knotbuffer))
  }
  out1 = outs[[1]]
    
  }
  else if (temporal ==2){# here!
    list1 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[1]], knotsT =round(length(tt)/4+2),meas)
    list2 = list(unsmooth = ss[term_path[[1]]])
    out1 = c(list1, list2)
  }else {
    out1 = list(unsmooth = ss[term_path[[1]]])
    out1$pred = out1$unsmooth
  }



  #Need to make sure the arclength now that it is in meters does not decrease because it is possible that flowline turns back slightly
  
  list = list(outs = outs, sSmooth = sSmooth)
  return(list)
  
}


temporal_smooth_mat <- function(obs, tt, knotT = min(round(length(tt)/4)+4, 35+4)){
  est = matrix( NA, nrow = nrow(obs), ncol = ncol(obs))
  rangT = c(tt[1], tt[length(tt)]); 
  knotsT <- quantile(tt[2:(length(tt)-1)], p = seq(0,1,length.out = knotT));
  bbasisT = create.bspline.basis(rangT, norder=6, breaks = knotsT)
  X = eval.basis(tt,bbasisT)
  for( ii in 1:ncol(obs)){  # loops through dates
    Slist = list(X=list(diag(ncol(X))))
    out = mgcv::gam(obs[,ii] ~ -1 + X, paraPen = Slist, family= gaussian(), method = "GCV.Cp")
    est[,ii] <- X %*% out$coefficients
  }
  return (list(est = est))
}

spatial_smooth <- function(obs, ss, knotS = min(round(length(ss)/4)+4, 35+4)){
  obs = t(obs) #make sure we have rows as intensity as same arc length
  est = dd1 =dd2= dd3 = se2 = matrix( NA, nrow = nrow(obs), ncol = ncol(obs))
  rangS = c(ss[1], ss[length(ss)]); 
  knotsS <- quantile(ss[2:(length(ss)-1)], p = seq(0,1,length.out = knotS));
  bbasisS = create.bspline.basis(rangS, norder=6, breaks = knotsS)
  X = eval.basis(ss,bbasisS)
  X1 = eval.basis(ss,bbasisS,1)
  X2 = eval.basis(ss,bbasisS,2)
  X3 = eval.basis(ss,bbasisS,3)
  for( ii in 1:ncol(obs)){  # loops through dates
    #print(ii)
    Slist = list(X=list(diag(ncol(X))))
    out = mgcv::gam(obs[,ii] ~ -1 + X, paraPen = Slist, family= gaussian(), method = "GCV.Cp")
    est[,ii] <- X %*% out$coefficients
    dd1[,ii] <- X1 %*% out$coefficients # Columns of dd1 correspond to dates
    dd2[,ii] <- X2 %*% out$coefficients
    dd3[,ii] <- X3 %*% out$coefficients
    sigma2 <- sum((obs[,ii] - est[,ii])^2)/(length(obs[,ii]) - ncol(X))
    ses <- diag( X2 %*% ginv( t(X) %*% X + out$sp*diag(ncol(X))) %*% t(X2) * sigma2 )
    se2[,ii] <- ses
  }
  
  return (list(dd1 = t(dd1), dd2 = t(dd2), dd3=t(dd3), est = t(est)))
}

terminus_paths <- function(dd1, yearTab, distanceTab, glacier, invert, distPerYear, n_paths){
  #Offset is 2 for the landsat 5 images and 1 for all landsat 7 and 8 images
  #Landsat 7 starts in 2000
  #Offset the correction for the edge effect on the derivative matrices
  # if(length(yearTab) < 200){
  #   offset = 2
  # } else {
  #   offset = 1
  # }
  offset = 1
  if(invert){dd1 = -dd1}

  term1 = terminus_est(dd1, yearTab, distanceTab,
                       invert, distPerYear, flip = 0, offset, n.top = n_paths)
  term2 = terminus_est(dd1, yearTab, distanceTab,
                       invert, distPerYear, flip = 1, offset, n.top = n_paths)

  pathCosts <- numeric()
  candidate_paths = cbind(term1,term2)
  for(i in 1:(2*n_paths)){
    if(i %in% c(1:n_paths)){
      pathCosts[i] = pathCost(dd1, candidate_paths[,i], flip = 0)
    }else{
      pathCosts[i] = pathCost(dd1, candidate_paths[,i], flip = 1)
    }
   
  } 
  
index = sort(pathCosts, index.return = TRUE, decreasing = TRUE)$ix
candidate_paths[, 11:20] <- flipud(candidate_paths[, 11:20])
out <- candidate_paths[, index]
colnames(out) <- paste0("candidate ", index)

# Compact return statement using lapply
return(c(lapply(index[1:20], function(i) candidate_paths[, i])))

  
}

terminus_est <- function(dd1, tt, ss, invert = 0, distPerYear, flip, offset, n.top = 5){
  if(flip){
    dd1 = flipud(dd1)
    tt = rev(tt)
  }
  #invert depends on the band we are working with
  #If the band is thermal band we are looking for positive changes in derivative unvert = 0
  #NDSI looking for negative changes in the derivative invert = 1
  # if(invert) {dd1 = -dd1}
  #moved to previous function
  #Just creating the empty dd1s to be filled
  maxVal = phonTools::zeros(dd1)
  valFrom = phonTools::zeros(dd1)
  jdiff = ss[2] - ss[1]
  row.start=ceiling(dd1[which.max(dd1)])
  #Looping through the dates
  for(i in 1:nrow(dd1)){
    #print(i)
    if(i == 1){
      maxVal[i,] = dd1[i,]
      valFrom[i,] = 1:ncol(dd1)
      next
    }else{
      #print(i)
      idiff = abs(tt[i-1] - tt[i])
    }

    #Looping through the dates


    #Time difference is idiff and jdiff is location difference
    #Distance per year is maximum meters to move per year
    jrange = (distPerYear * idiff) / (jdiff)
    #print(jrange)
    #Loops through the locations
    # skips first and last 3? for what reason
    for(j in (1 + offset):(ncol(dd1)-offset)){
      #Why are we subtracting 3 off each end here?
      #minbound and maxbound restrict how far the terminus can move
      minBound = max(j - jrange, (1 + offset))
      #print(j - jrange)
      maxBound = min(j + jrange, (ncol(dd1) - offset))
      #print(j + jrange)


      #All columns between min and maxbound calculated before
      #Find the maximum value and what column it came from in prev and prevFrom
      max.ix <- which.max(c(maxVal[(i-1), round(minBound):round(maxBound)]))
      prev <- max(c(maxVal[(i-1), round(minBound):round(maxBound)]))
      prevFrom <- (round(minBound):round(maxBound))[max.ix]

      maxVal[i, j] = prev + dd1[i, j]
      valFrom[i, j] = prevFrom
    }
  }

  #Identify columns in last row with the five highest values

  ind <- sort(maxVal[nrow(maxVal),], index.return=TRUE, decreasing=TRUE)$ix[1:n.top]

  ValsFromMax =valFrom[nrow(maxVal),ind]
  terminusPaths = matrix(nrow = nrow(maxVal), ncol = n.top)
  y = 1
  for(k in ind){
    terminusPaths[nrow(valFrom),y] = ValsFromMax[y]
    curr = valFrom[nrow(valFrom),k]
    for(i in (nrow(valFrom)-1):1){
      terminusPaths[i,y] = curr
      curr = valFrom[i,curr]
    }
    y = y + 1
  }


  return(terminusPaths)
}


pathCost <- function(dd1, path, flip){
  cost = 0
  if(flip){
    dd1 = flipud(dd1)
  }
  
  for(i in 1:length(path)){
    cost = cost + dd1[i,path[i]]
  }
  
  return(cost)
}

temporal_smooth <- function(ss,tt, est,dd3,term_path, knotsT =-1){
  #evaluate at path, time point k
  muT = function( obj, term_path, k){ obj[k,term_path[k]]}
  wts <- sapply( seq(1,nrow(dd3),1), muT, obj = dd3, term_path = term_path)
  wts[wts < 0] = 1e-06
  if (length(unique(ss[term_path])) == 1) {
    b <- knots <- out <- pred <- predSe <- pred.ks <- predSe.ks <- predMeas <- predMeasSe <- NULL
    pred <- ss[term_path]
    predSe <- rep(NA, length(pred))
    pred.ks <- rep(NA, length(pred))
  } else {
    b <- mgcv::gam(ss[term_path] ~ s(tt, bs = "cr", k = knotsT) , weights = wts, method = "REML")
    knots <- b$smooth[[1]]$xp
    out <- predict(b, se.fit = TRUE)
    ### predicted path values
    pred <- out$fit
    predSe <- out$se.fit # Standard error is from smoothing step, se of the gam after getting the path
    # TODO: use out$vc for the estimated covariance for the parameters
    Xp <- predict(b, type="lpmatrix")
    err <- list(lpmatrix = Xp,
                Vp = b$Vp,
                se = predict(b,se.fit = TRUE)$se.fit,
                sp = b$sp,
                coeffs = coef(b),
                fit_1 = b
                )
    
    # ksmooth instead of predictions
    pred.ks <- ksmooth(tt, ss[term_path], bandwidth = 5, kernel = "normal", n.points = length(tt))$y
    predSe.ks <- NULL
    
    ### predicted values over ground measurement times
    ### This is used to match datetime between ground measurement and the image dates
  }
  
  
  return(list(unsmooth = ss[term_path],
              knots = knots, wts = wts
               , err = err, pred = pred))

}

terminus_plot <- function(direc,glacier,ss,tt,obs,out1,out2, out3,out4, out5, out6, out7, out8, out9, out10, sSmooth,line.fit,meas,measAdj,temporal,linefit = 1,invert, knotbuffer = 1, newmethod = FALSE) {
  col_B61<- colorRampPalette(c("tan4", "lightblue2"))
  
  write.csv(line.fit, paste(direc, glacier, "line_fit.csv", sep = ""))
  #setwd(direc)
  # plot 1 - profile intensity
  main = paste(direc,glacier,"_ndsi_intensityprofile.png", sep = "")
  png(main, width = 6.5, height = 6, units = 'in', res = 300)
  matplot(ss, t(obs), col = "grey", type = "l", ylab = "Profile intensity", xlab = "Distance along the glacial flowline (meters)", cex.axis = 1.5, cex.lab = 1.5)
  dev.off()

  # plot 2 - first derivative
  #Trying to fix this plot
  dd1 = sSmooth$dd1
  main = paste(direc,glacier,"_ndsi_firstderivativecandidate.png", sep = "")
  png(main, width = 6.5, height = 6, units = 'in', res = 300)
  par(oma=c( 0,1,0,0))
  dmax = quantile(abs(dd1), .99)
  dd1[which(dd1 > dmax)] = dmax
  dd1[which(dd1 < -dmax)] = -dmax
  #Need to make sure the arclength now that it is in meters does not decrease because it is possible that flowline turns back slightly
  #Make sure fonts are the same size and that the labels match between intensity terminus
  #plot and the derivative plot
  library(grDevices)
  cols = colorRampPalette(c(muted("blue"), "white", muted("red")))
  col_pal = cols(64)
  image.plot(tt, ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year", col = col_pal)
  
  if(temporal ==  0){
    lines(tt, out1$unsmooth, col = "black", lwd = 3.5)
  }else if (temporal ==2){
    lines(tt, out1$unsmooth,  col = "yellow", lwd = 3.5)
    lines(tt,  out1$pred, col = "black", lwd = 3.5)
    lines( tt, out1$pred + 2*out1$predSe, lwd = 1.5, col = "purple")
    lines(  tt, out1$pred - 2*out1$predSe,lwd = 1.5, col = "purple")
  }else{
    lines(tt, out1$pred,  col = "black", lwd = 3.5)
    #lines(tt,  out1$pred + 2*out1$predSe, lwd = 1.5, col = "purple")
    #lines( tt, out1$pred - 2*out1$predSe, lwd = 1.5, col = "purple")
  }
  dev.off()
  col_B61 <- colorRampPalette(c("tan4", "lightblue2"))( 500 )
  
  # plot 3 - estimate terminus
  if(invert == 1){
    col <- col_B61
  }else {
    col <- rev(col_B61)
  }
  #col = rev(col_B61)
  main = paste(direc,glacier,"_ndsi_terminusestimatecandidate.png", sep = "")
  png(main, width = 6.5, height = 6, units = 'in', res = 300)
  par(oma=c( 0,1,0,0))
  obsmat = as.matrix(obs)
  omax = quantile(abs(obs), .99, na.rm = T)
  omin = quantile(abs(obs), .01, na.rm = T)
  obs[which(obs > omax)] = omax
  obs[which(obs < omin)] = omin
  image.plot(tt, ss, obs, zlim = c(omin, omax), xlab = "Year", ylab = "Flowline arclength (meters)", cex.axis = 1.5, cex.lab = 1.5, col = col)
  if(temporal ==  0){
    lines( out1$unsmooth, tt, col = "yellow", lwd = 3.5)
  }else if (temporal ==2){
    lines( out1$unsmooth, tt, col = "yellow", lwd = 3.5)
    lines( out1$pred, tt, col = "black", lwd = 3.5)
    lines( out1$pred + 2*out1$predSe,tt, lwd = 1.5 , col = "purple")
    lines( out1$pred  - 2*out1$predSe,tt, lwd = 1.5, col = "purple")
  }else{
    lines(tt,  out1$pred, col = "yellow", lwd = 3.5)
    #lines( tt,out1$pred + 2*out1$predSe, lwd = 1.5 , col = "purple")
    #lines( tt, out1$pred  - 2*out1$predSe,lwd = 1.5, col = "purple")
  }
  #if(linefit == 1){
  lines(tt, line.fit, lwd = 2, col = "green")
  #}
  if(!is.null(meas)){
    points(as.numeric(measAdj[,1]), measAdj[,2],pch = 16, col = "red")
  }
  dev.off()
  #Derivative plot with 3 paths 
  #Trying to fix this plot
  dd1 = sSmooth$dd1
  main = paste(direc,glacier,"_ndsi_fivepathcandidates.png", sep = "")
  png(main, width = 6.5, height = 6, units = 'in', res = 300)
  par(oma=c( 0,1,0,0))
  dmax = quantile(abs(dd1), .99)
  dd1[which(dd1 > dmax)] = dmax
  dd1[which(dd1 < -dmax)] = -dmax
  #Need to make sure the arclength now that it is in meters does not decrease because it is possible that flowline turns back slightly
  
  image.plot( tt,ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year")
  
  if(temporal ==  0){
    lines(tt, out1$unsmooth,  col = "black", lwd = 3.5)
  }else if (temporal ==2){
    lines(tt, out1$unsmooth,  col = "purple", lwd = 3.5)
    lines(  tt, out1$pred,col = "black", lwd = 3.5)
    lines(  tt, out1$pred + 2*out1$predSe,lwd = 1.5, col = "purple")
    lines( tt, out1$pred - 2*out1$predSe, lwd = 1.5, col = "purple")
  }else{
    #lines(  tt,out1$pred + 2*out1$predSe, lwd = 1.5, col = "purple")
    #lines(  tt,out1$pred - 2*out1$predSe, lwd = 1.5, col = "purple")
    
    lines(  tt,out2$pred, col = "grey", lwd = 3.5)
    #lines(  tt, out2$pred + 2*out2$predSe,lwd = 1.5, col = "black")
    #lines(  tt, out2$pred - 2*out2$predSe,lwd = 1.5, col = "black")
    
    
    lines( tt, out3$pred, col = "white", lwd = 3.5)
    #lines(  tt,out3$pred + 2*out3$predSe, lwd = 1.5, col = "black")
    #lines(  tt,out3$pred - 2*out3$predSe, lwd = 1.5, col = "black")
    
    lines( tt, out4$pred, col = "white", lwd = 1)
    #lines(  tt,out4$pred + 2*out4$predSe, lwd = 1.5, col = "black")
    #lines(  tt,out4$pred - 2*out4$predSe, lwd = 1.5, col = "black")
    
    lines( tt, out5$pred, col = "white", lwd = 1)
    #lines(  tt,out5$pred + 2*out5$predSe, lwd = 1.5, col = "black")
    #lines(  tt,out5$pred - 2*out5$predSe, lwd = 1.5, col = "black")
    
    lines(  tt,out6$pred, col = "white", lwd = 1)
    lines(  tt,out7$pred, col = "white", lwd = 1)
    lines(  tt,out8$pred, col = "white", lwd = 1)
    lines(  tt,out9$pred, col = "white", lwd = 1)
    lines(  tt,out10$pred, col = "white", lwd = 1)
    
    lines(tt,  out1$pred, col = "black", lwd = 3.5)
    
  }
  dev.off()
  col_B61 <- colorRampPalette(c("tan4", "lightblue2"))( 500 )
  
  # # plot 4 - zoomed estimate terminus
  # range = range(out1$unsmooth)
  # lower = range[1] - 3*(range[2]-range[1])
  # lower = max(lower,min(ss))
  # upper = range[2] + 3*(range[2]-range[1])
  # upper = min(upper, max(ss))
  # 
  # main = paste(glacier,"terminus estimate zoomed candidate")
  # png(paste(main,'.png', sep =""), width = 6.5, height = 6, units = 'in', res = 300)
  # par(oma=c( 0,1,0,0))
  # obsmat = as.matrix(obs)
  # omax = quantile(abs(obsmat), .99,na.rm = T)
  # omin = quantile(abs(obsmat), .01,na.rm = T)
  # obsmat[which(obsmat > omax)] = omax
  # obsmat[which(obsmat < omin)] = omin
  # image.plot( ss, tt, obsmat,  col = col, xlim = c(lower, upper), ylim = c(min(tt), max(tt)), zlim = c(omin, omax), xlab = "Distance along the glacial flowline (meters)", ylab = "Year", cex.axis = 1.5, cex.lab = 1.5)
  # if(temporal ==  0){
  #   lines( out1$unsmooth, tt, col = "yellow", lwd = 3.5)
  # }else if(temporal ==2){
  #   lines( out1$unsmooth, tt, col = "yellow", lwd = 3.5)
  #   lines( out1$pred, tt, col = "black", lwd = 3.5)
  #   lines( out1$pred + 2*out1$predSe,tt, lwd = 1.5 , col = "purple")
  #   lines( out1$pred  - 2*out1$predSe,tt, lwd = 1.5, col = "purple")
  # }else{
  #   lines( out1$pred, tt, col = "yellow", lwd = 3.5)
  #   lines( out1$pred + 2*out1$predSe,tt, lwd = 1.5 , col = "purple")
  #   lines( out1$pred  - 2*out1$predSe,tt, lwd = 1.5, col = "purple")
  # }
  # if( linefit == 1){
  #   lines( line.fit, lwd = 2, tt, col = "green")
  # }
  # if(!is.null(meas)){
  #   points( measAdj[,2], measAdj[,1], lwd = 3, col = "red")
  # }
  # dev.off()
  # list(lower = unname(omin), upper = unname(omax))

}