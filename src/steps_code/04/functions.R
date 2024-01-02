get_FV_joined_file <- function() {
    work_dir_path = config$work_dir_path
    joined_csv_path = paste0(work_dir_path,"/src/FV_joined.csv")
    FV <- read.csv(joined_csv_path, header = T)
    return(FV)
}

arclength <- function(path, dem){
  xmin = extent(dem)@xmin
  xmax = extent(dem)@xmax
  ymin = extent(dem)@ymin
  ymax = extent(dem)@ymax
  
  
  distances = c()
  #Already in meters
  distances = pointDistance(path, lonlat = FALSE, allpairs = T)
  distance = numeric()
  for(i in 1:(nrow(distances)-1)){
    distance[i] = distances[i, (i+1)]
  }
 
  arclength=c(0, cumsum(distance))
  
  return(arclength)
}

terminus <- function(glacier, obs, ss, tt, meas= NULL, plot = FALSE, direc = NULL, linefit = 1, temporal = 1,invert = 1, distPerYear, knotbuffer = 1){
  # tSmooth <- temporal_smooth_mat(obs, tt, knotT = min(round(length(tt)/4)+4, 35+4))$est
  knotbuffer = min(4, knotbuffer) # this is set to be 4 at maximum because the knots may be too little when setting spacing too large
  tSmooth <- temporal_smooth_mat(obs, tt, knotT = round(diff(range(tt))/knotbuffer))$est
  # tSmooth <- obs
  # sSmooth = spatial_smooth(tSmooth, ss, knotS = min(round(diff(range(ss))/3 0), 39))
  sSmooth = spatial_smooth(tSmooth, ss, knotS = min(round(length(ss)/4)+4, 35+4))
  
  
#-------------------------------------------------------------------------------
  #source("~/Glaciers 2/DataForShashank/terminus_est.R")
  term_path = terminus_paths(sSmooth$dd1,tt,ss,glacier,invert=invert,distPerYear)
#----------------------------------------------------------------------------
  if (temporal == 1) {
    # out1 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[1]], knotsT =round(length(tt)/30),meas
    out1 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[1]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out2 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[2]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out3 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[3]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out4 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[4]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out5 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[5]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out6 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[6]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out7 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[7]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out8 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[8]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out9 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[9]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out10 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[10]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out11 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[11]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out12 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[12]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out13 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[13]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out14 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[14]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out15 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[15]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out16 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[16]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out17 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[17]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out18 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[18]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out19 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[19]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    out20 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[20]], knotsT =round(diff(range(tt))/knotbuffer),meas)
    
    }else if (temporal ==2){# here!
    list1 = temporal_smooth(ss,tt,est = sSmooth$est, dd3 =sSmooth$dd3, term_path = term_path[[1]], knotsT =round(length(tt)/4+2),meas)
    list2 = list(unsmooth = ss[term_path[[1]]])
    out1 = c(list1, list2)
  }else {
    out1 = list(unsmooth = ss[term_path[[1]]])
    out1$pred = out1$unsmooth
  }
  
  
  
  
  prev_wd = getwd()
  #setwd("~/Documents/GitHub/Glacier-R-Code/Plots/") #Commented by shashank
  items = as.matrix(t(c(glacier, term_path[[6]], mean(rowSds(obs)), term_path[[6]]/mean(rowSds(obs)), ncol(obs))))

  
  # TODO: Uncomment
  # write.table(items, file = "costs_sd.csv", append = TRUE, quote = FALSE,
  # col.names = FALSE, row.names = FALSE)
  
  change = as.data.frame(as.matrix(t(c(glacier, out1$pred[1], out1$pred[length(out1$pred)],out1$pred[length(out1$pred)] - out1$pred[1], tt[length(tt)] - tt[1], term_path[[11]]/(mean(rowSds(obs))*ncol(obs))))))
 
   # TODO: Uncomment
  # write.table(change, file = "change_overtime.csv", append = TRUE, quote = FALSE, row.names = FALSE, sep = ",", col.names = c("GID", "start", "end", "change", "time", "cost"))
  #setwd(prev_wd) commented by Shashank
  #Need for the second two as well 
  
  
  # fit a line through the terminus locations
  # *** This step needs to be restored, but term_path and tt do not have the same lengths
  if (temporal == 1) {
    lm = lm(out1$pred ~ tt) # fit a line through the terminus
  }else { # here! fit a line through unsmoothed path
   lm = lm(out1$unsmooth ~ tt) # fit a line through the terminus
  }
  slope= coef(lm)[2]
  
  #Sandwich estimator for variance of the slope estimate for robustness
  #NEed to add the weights 
  
  #X <- model.matrix(lm)
  #XX1 <- solve(t(X)%*%X) #X'X inverse
  
  #XX1 <- solve(t(X)%*%W%*%X)
  #u2 <- residuals(lm)^2 #(residuals^2)
  #XDX <- 0 
  #Go to the source to summarize the math
  # for(i in 1:nrow(X)) {
  #   XDX <- XDX + (out1$predSe[i]^2)*X[i,]%*%t(X[i,])
  # }
  
  # Variance calculation (Bread x meat x Bread)
  #varcovar <- XX1 %*% XDX %*% XX1
  
  # degrees of freedom adjustment
  #dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
  
  # Standard errors of the coefficient estimates are the
  # square roots of the diagonal elements
  #stdh <- dfc*sqrt(diag(varcovar))
  
  #slope.se = stdh[2]
  slope.rmse = sqrt(mean((lm$fitted.values - out1$pred)^2))
  #old naive estimate
  slope.se= summary(lm)$coef[2,2]
  line.fit = lm$fitted.values
  # slope = 0 # Being assigned to 0 and so there is no slope or se
  # slope.se = 0
  # line.fit = out1$pred
  
  
  # Ground measurement calculations
  if(!is.null(meas)){
    ind = which((meas[,1] > floor(min(tt)))*(meas[,1]<ceiling(max(tt)))==1)
    adj <- mean(out1$predMeas[ind]) - mean(cumsum(meas[ind,2]))
    measAdj <- data.frame(tt = meas[ind,1], gm = cumsum(meas[ind,2]) + adj)
    MIAE <- mean( abs(out1$predMeas[ind] - measAdj[,2]))
    #Need empirical coverage of the confidence bands
    MAD <- max( abs(out1$predMeas[ind] - measAdj[,2]) )
    nMeas <- length(measAdj[,2])
  }
  
  if(is.null(meas)){
    mat <- matrix(c(slope,slope.se,slope.rmse, NA, NA, NA), nrow = 1)
    MIAE = NA
    MAD = NA
    nMeas = 0
  }else{
    mat <- matrix(c(slope,slope.se,slope.rmse, MIAE, MAD, nMeas), nrow = 1)
  }
  colnames(mat) <- c("slope","slope.se","slope.rmse", "MIAE", "MAD", "nMeas")
  #print (mat)

  ##Commented by Shashank 02/01/24
#   if(!is.na(mat[3])){
#     write.table(mat,paste0(path,"Plots/",glacier,"/",glacier,".txt"),col.name =TRUE,row.name=FALSE)
#   }
  
  
  if(plot==1){
    bounds = terminus_plot(direc,glacier,ss,tt,obs,out1,out2, out3,out4,out5, out11, out12, out13, out14, out15, sSmooth,line.fit,meas = meas,measAdj = measAdj,temporal,line.fit,invert, knotbuffer = knotbuffer)
    #write.table(obs, file = paste(glacier, "unsmoothed intensity.txt"), row.names =FALSE, col.names =FALSE)
    #write.table(tt, file = paste(glacier, "year.txt"), row.names =FALSE ,col.names =FALSE)
    #write.table(ss, file = paste(glacier, "distance.txt"), row.names =FALSE, col.names =FALSE)
  }
  
  #list = list(line.fit = line.fit, pred = out1$pred, predSe = out1$predSe, slope = slope, slopeSe = slope.se, dd1 = sSmooth$dd1,
              #MIAE = MIAE, MAD = MAD, lower = bounds$lower, upper = bounds$upper)

  #Need to make sure the arclength now that it is in meters does not decrease because it is possible that flowline turns back slightly
  
  list = list(line.fit = line.fit, pred = out1$pred, predSe = out1$predSe, slope = slope, slopeSe = slope.se, dd1 = sSmooth$dd1,
              MIAE = MIAE, MAD = MAD, pred2 = out2$pred, pred3 = out3$pred, pred4 = out4$pred, pred5 = out5$pred, unsmooth = ss[term_path[[1]]]
              , indice_path = term_path[[1]], pred.ks = out1$pred.ks, predSe.ks = out1$predSe.ks)
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
  
  saveRDS(dd2,
          file = paste0(glacier, "dd2_output.rds"))
  return (list(dd1 = t(dd1), dd3=t(dd3), est = t(est)))
}

terminus_paths <- function(dd1, yearTab, distanceTab, glacier, invert, distPerYear){
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
  # saveRDS(list(dd1 = -dd1, tt = yearTab, ss = distanceTab, invert = invert,
  #              distPerYear = distPerYear, flip = 0, offset = offset, n.top = 10),
  #         file = paste0(glacier, "terminus_est_input.rds"))
  term1 = terminus_est(dd1, yearTab, distanceTab,
                       invert, distPerYear, flip = 0, offset, n.top = 10)
  # saveRDS(term1,
  #         file = paste0(glacier, "terminus_est_front_output.rds"))
  # print("term 1")
  
  term2 = terminus_est(dd1, yearTab, distanceTab,
                       invert, distPerYear, flip = 1, offset, n.top = 10)
  # print("term2")
  # saveRDS(term2,
  #         file = paste0(glacier, "terminus_est_back_output.rds"))
  
  pathCosts <- numeric()
  
  candidate_paths = cbind(term1,term2)
  for(i in 1:20){
    if(i %in% c(1:10)){
      pathCosts[i] = pathCost(dd1, candidate_paths[,i], flip = 0)
    }else{
      pathCosts[i] = pathCost(dd1, candidate_paths[,i], flip = 1)
    }
   
  } 
  print(pathCosts)
  
  index=sort(pathCosts, index.return=TRUE, decreasing=TRUE)$ix
  # print(index)
  # print(candidate_paths[,index[1]])
  candidate_paths[,11:20] <- flipud(candidate_paths[,11:20])
  out <- candidate_paths[,index]
  colnames(out) <- paste0("candidate ", index)
  write.csv(out, paste0("~/",
                        glacier, "_Path_index.csv"), row.names = FALSE)

  return(list(candidate_paths[,index[1]], 
              candidate_paths[,index[2]], 
              candidate_paths[,index[3]],
              candidate_paths[,index[4]], 
              candidate_paths[,index[5]], 
              candidate_paths[,index[6]], 
              candidate_paths[,index[7]], 
              candidate_paths[,index[8]],
              candidate_paths[,index[9]], 
              candidate_paths[,index[10]], 
              candidate_paths[,index[11]], 
              candidate_paths[,index[12]], 
              candidate_paths[,index[13]],
              candidate_paths[,index[14]], 
              candidate_paths[,index[15]], 
              candidate_paths[,index[16]], 
              candidate_paths[,index[17]], 
              candidate_paths[,index[18]],
              candidate_paths[,index[19]], 
              candidate_paths[,index[20]],
              pathCosts[index[1]]
              ))
  
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

