col_list = c('black','red','yellow','green', 'blue','pink','brown', 'purple', 'cyan', 'magenta', 'grey', 'darkgreen', 'darkblue', 'lightblue', 'magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta','magenta')

calculate_covariance_matrix <- function(i, outs) {
  SE = outs[[paste0("out", i)]]$err

  if(is.null(SE))
    return(NULL)
  #covariance matrix
  V_cov <- SE$Vp
  
  #phi design matrix
  Phi <- SE$lpmatrix
  
  # squared SE
  squared_SE_matrix <- Phi %*% V_cov %*% t(Phi)
  
  return(squared_SE_matrix)
}

reachabilityPlot<- function(glacier, res){
  reachDist = res$reachdist
  reachDist[[1]] = 0


  kc = res$cluster
  kc = replaceZeros(kc)
  plot(1, type="n",xlim = c(1, length(res$order)), ylim = c(0, max(reachDist)),
       xlab = "Points", ylab = "Reachability Distance", main = paste(glacier,"Reachability Plot"))


  for (i in seq_along(res$order)) {
    idx = res$order[i]
    color <- col_list[kc[idx]]  # Color based on the cluster assignment
    segments(i,0,i,reachDist[idx], col = color, lwd = 2)
  }
  
  abline(h = res$eps_cl, col='red', lty='dashed')
}

# reachabilityPlot <- function(glacier, res, num_points) {
#   reachDist = res$reachdist / num_points 
#   reachDist[[1]] = 0

#   kc = res$cluster
#   kc = replaceZeros(kc)
#   plot(1, type = "n", xlim = c(1, length(res$order)), ylim = c(0, max(reachDist)),
#        xlab = "Points", ylab = "Average Reachability Distance", main = paste(glacier, "Reachability Plot"))

#   for (i in seq_along(res$order)) {
#     idx = res$order[i]
#     color <- col_list[kc[idx]]  #
#     segments(i, 0, i, reachDist[idx], col = color, lwd = 2)
#   }

#   abline(h = res$eps_cl / num_points, col = 'red', lty = 'dashed') 
# }


replaceZeros <- function(arr) {
  max_num <- max(arr)
  zero_positions <- which(arr == 0)
  
  for (i in seq_along(zero_positions)) {
    arr[zero_positions[i]] <- max_num + i
    #max_num = max_num + i
  }
  
  return(arr)
}

plot_clustered_paths <- function(glacier, dd1,tt,ss,all_path_list,kc, min_cost_indices){
  print(kc)
  kc = replaceZeros(kc)
  kc = generate_color_kc(kc)
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
    # Check if the current path is a minimum cost path within its cluster
    if (i %in% min_cost_indices) {
      line_width =2  # Thicker line for minimum cost path
    } else {
      line_width = 1  # Normal line otherwise
    }
    lines(tt, all_path_list[[i]],  col = col_list[[kc[[i]]]], lwd = line_width)
  }

}


calculate_mean_std_curves <- function(all_path_list, kc) {
  kc = replaceZeros(kc)
  cluster_ids <- unique(kc)
  curves_list <- list()

  for (cluster_id in cluster_ids) {
    cluster_indices <- which(kc == cluster_id)
    cluster_paths <- all_path_list[cluster_indices]

    if (length(cluster_paths) == 1) {
      # Handle the one-dimensional case
      mean_curve <- cluster_paths[[1]] # The mean curve is the curve itself
      std_curve <- rep(0, length(mean_curve)) # Standard deviation is a vector of zeros
    } else {
      # Handle the general case
      mean_curve <- rowMeans(do.call(cbind, cluster_paths))
      std_curve <- sqrt(rowSums(sapply(cluster_paths, function(path) (path - mean_curve)^2)) / (length(cluster_paths) - 1)) # Note the -1 for Bessel's correction
    }

    curves_list[[cluster_id]] <- list(mean = mean_curve, std = std_curve)
  }

  return(curves_list)
}

plot_clustered_mean_std <- function(glacier, dd1, tt, ss, curves_list, col_list) {
  
  par(oma=c(0, 1, 0, 0))
  dmax <- quantile(abs(dd1), .99)
  dd1[which(dd1 > dmax)] <- dmax
  dd1[which(dd1 < -dmax)] <- -dmax
  
  cols <- colorRampPalette(c(muted("blue"), "grey", muted("red")))

  
  image.plot(tt, ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year", col = col_pal, main = paste(glacier, "Clustered Mean and STD paths"))

  for (i in 1:length(curves_list)) {
    mean_curve <- curves_list[[i]]$mean
    std_curve <- curves_list[[i]]$std
    
    # Mean Curve
    lines(tt, mean_curve, col = col_list[i], lwd = 2)
    
    # 1 Standard Deviation Shading
    polygon(c(tt, rev(tt)), c(mean_curve + std_curve, rev(mean_curve - std_curve)), col = adjustcolor(col_list[i], alpha.f = 0.5), border = NA)
    
    # 2 Standard Deviations Shading
    polygon(c(tt, rev(tt)), c(mean_curve + 2 * std_curve, rev(mean_curve - 2 * std_curve)), col = adjustcolor(col_list[i], alpha.f = 0.3), border = NA)
  }
  
}

