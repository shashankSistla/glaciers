col_list = c('black','red','yellow','green', 'blue','pink','brown', 'purple', 'cyan', 'magenta', 'grey', 'darkgreen', 'darkblue')

temporal_smooth <- function(ss,tt, dd3,term_path, knotsT =-1){
  #evaluate at path, time point k
  muT = function( obj, term_path, k){ obj[k,term_path[k]]}
  wts <- sapply( seq(1,nrow(dd3),1), muT, obj = dd3, term_path = term_path)
  wts[wts < 0] = 1e-06
  if (length(unique(ss[term_path])) == 1) {
    print("am i in the if condition")
    b <- knots <- out <- pred <- predSe <- pred.ks <- predSe.ks <- predMeas <- predMeasSe <- NULL
    pred <- ss[term_path]
    predSe <- rep(NA, length(pred))
    pred.ks <- rep(NA, length(pred))
    err <- NULL
    
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


plot_smoothened_paths <- function(glacier, dd1,tt,ss,all_path_list){
  cols = colorRampPalette(c(muted("blue"), "grey", muted("red")))
  col_pal = cols(64)

  par(oma=c( 0,1,0,0))
  dmax = quantile(abs(dd1), .99)
  dd1[which(dd1 > dmax)] = dmax
  dd1[which(dd1 < -dmax)] = -dmax

  image.plot( tt,ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year",col = col_pal, main=paste(glacier, "Clustered paths"))

# Using sapply to compute minimum cost indices


  # Plotting paths
  for (i in seq_along(all_path_list)) {
    lines(tt, all_path_list[[i]]$pred,  col = col_list[[i]], lwd = 1.5)
  }

}