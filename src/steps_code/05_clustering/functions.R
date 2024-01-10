col_list = c('black','red','yellow','green', 'blue','pink','brown', 'purple', 'cyan', 'magenta', 'grey', 'darkgreen', 'darkblue')

calculate_covariance_matrix <- function(i, outs) {
  SE = outs[[paste0("out", i)]]$err
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

replaceZeros <- function(arr) {
  max_num <- max(arr)
  zero_positions <- which(arr == 0)
  
  for (i in seq_along(zero_positions)) {
    arr[zero_positions[i]] <- max_num + i
    #max_num = max_num + i
  }
  
  return(arr)
}

plot_clustered_paths <- function(glacier, dd1,tt,ss,all_path_list,kc){
  kc = replaceZeros(kc)
  cols = colorRampPalette(c(muted("blue"), "grey", muted("red")))
  col_pal = cols(64)

  par(oma=c( 0,1,0,0))
  dmax = quantile(abs(dd1), .99)
  dd1_2 = dd1
  dd1[which(dd1 > dmax)] = dmax
  dd1[which(dd1 < -dmax)] = -dmax

  image.plot( tt,ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year",col = col_pal, main=paste(glacier, "Clustered paths"))
  for (i in seq_along(all_path_list)) {
    lines(tt, all_path_list[[i]], col = col_list[[kc[[i]]]], lwd = 2)
  }
}


plot_clustered_mean_std <- function(glacier, dd1, tt, ss, all_path_list, kc, col_list) {
  
  cluster_ids <- unique(kc)
  
  par(oma=c(0, 1, 0, 0))
  dmax <- quantile(abs(dd1), .99)
  dd1[which(dd1 > dmax)] <- dmax
  dd1[which(dd1 < -dmax)] <- -dmax
  
  cols <- colorRampPalette(c(muted("blue"), "grey", muted("red")))
  col_pal <- cols(64)
  
  image.plot(tt, ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year", col = col_pal, main = paste(glacier, "Clustered Mean and STD paths"))
  
  for (cluster_id in cluster_ids) {
    cluster_indices <- which(kc == cluster_id)
    cluster_paths <- all_path_list[cluster_indices]
    mean_curve <- rowMeans(do.call(cbind, cluster_paths))
    std_curve <- sqrt(rowSums(sapply(cluster_paths, function(path) (path - mean_curve)^2)) / length(cluster_paths))
    
    # Mean Curve
    lines(tt, mean_curve, col = col_list[cluster_id], lwd = 2)
    
    # 1 Standard Deviation Shading
    polygon(c(tt, rev(tt)), c(mean_curve + std_curve, rev(mean_curve - std_curve)), col = adjustcolor(col_list[cluster_id], alpha.f = 0.5), border = NA)
    
    # 2 Standard Deviations Shading
    polygon(c(tt, rev(tt)), c(mean_curve + 2 * std_curve, rev(mean_curve - 2 * std_curve)), col = adjustcolor(col_list[cluster_id], alpha.f = 0.3), border = NA)
  }
  
}
