library(scales)
library(grDevices)
library(fields)
glacier = "G077333E32259N"
setwd("C:/Users/shash/OneDrive/Desktop/Research/Glacier Revamp/output/restart/03_extract_IP/output")
ss = readRDS(paste0(glacier,"_al.rds"))
tt = readRDS(paste0(glacier,"_dates_cut.rds"))





setwd("C:/Users/shash/OneDrive/Desktop/Research/Glacier Revamp/output/restart/04_candidate_paths/output")
candidate_paths = readRDS(paste0(glacier,"_candidate_paths.rds"))
path_costs = readRDS(paste0(glacier,"_path_costs.rds"))
sSmooth = readRDS(paste0(glacier,"_sSmooth.rds"))
n_paths = 10

dd1 = sSmooth$dd1

outs <- lapply(candidate_paths, function(indices) {
  ss[indices]
})


cols = colorRampPalette(c(muted("blue"), "grey", muted("red")))
col_pal = cols(64)
par(oma=c( 0,1,0,0))
dmax = quantile(abs(dd1), .99)
dd1[which(dd1 > dmax)] = dmax
dd1[which(dd1 < -dmax)] = -dmax
image.plot( tt,ss, dd1, zlim = c(-dmax, dmax), ylab = "Flowline arclength (meters)", xlab = "Year",col = col_pal, main=paste("", "Candidate Paths"))

min_cost_index_1 = which.max(path_costs[1:n_paths])
min_cost_index_2 = which.max(path_costs[(n_paths + 1):(2*n_paths)]) + n_paths

lines(tt, outs[[min_cost_index_1]], lwd = 3.5, col = "green")
lines(tt, outs[[min_cost_index_2]], lwd = 3.5, col = "yellow")

# Plot the other paths
for(i in 1:n_paths){
  if (i != min_cost_index_1) {
    print(i)
    lines(tt, outs[[i]], lwd = 1.5, col = "green")
  }
}
for(i in (n_paths+1):(2*n_paths)){
  if (i != min_cost_index_2) {
    print(i)
    lines(tt, outs[[i]], lwd = 1.5, col = "yellow")
  }
}

