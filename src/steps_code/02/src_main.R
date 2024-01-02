main.function_02 <- function(key, root_dir){
step_name = "02_GD_flowline"

library(raster)
library(matrixStats)
library(stats)
library(mgcv)

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/output/",key,"/02_GD_flowline/glacier_list.R"))
source(paste0(root_dir, "/src/steps_code/02/functions.R"))
source(paste0(root_dir, "/src/base_functions.R"))

work_dir_path = config$work_dir_path

step_01_output_dir = paste0(root_dir, "/output/", key, "/01_prepare_dem/output/")
output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
create_directory(output_dir, "output")
for(glacier in glacier_list){

    glacier_path = paste0(step_01_output_dir, glacier,"_dem.tif")
    initial.coord = readRDS(paste0(step_01_output_dir, glacier, "_initial_coord.rds"))
    dem = raster(glacier_path)
    step.size = ((dim(dem)[1] + dim(dem)[2])/2)*30/1200
    window.multiplier = ifelse(floor(step.size/5) < 1, 1, ifelse((step.size/5 - floor(step.size/5)) < 0.5, floor(step.size/5), ceiling(step.size/5)))
    window.multiplier = ifelse(step.size/5 < 0.5, 0.5, window.multiplier)

    gdOutput = GD.linear.sample(dem, initial.coord, step.size = step.size, blocks = window.multiplier*c(30,40,50,60))

    #Get parallel paths
    coord.parallel = path_parallel(gdOutput$coord, dem)
    warnings = gdOutput$warning
    #flag.df[flag.df$GID == glacier, names(warnings)] = as.numeric(warnings)
    #write.csv(flag.df, "WarningMessages.csv", row.names = FALSE)
    #This next if statement gets rid of points that fall outside the boundaries of the DEM image
    #There are NAs on the edges of the DEM that means sometimes the flowline will extend into that section
    #So we remove them
    indx <- min(which(is.na(raster::extract(boundaries(dem), coord.parallel[,1:2])) | raster::extract(boundaries(dem), coord.parallel[,1:2]) > 0))
    if(indx > 0 & indx < Inf){
        coord.parallel = coord.parallel[1:(indx-1),]
    }

    
    coord_parallel_filename = paste0(output_dir,"/output/", glacier,"_coord_parallel.rds")
    saveRDS(coord.parallel, file = coord_parallel_filename)

    #check if we need to plot
    flowline_plot_filename = paste0(output_dir, "/output/",glacier,"_flowline_dem.png")
    col_B61 <- colorRampPalette(c("tan4", "lightblue2"))

    png(flowline_plot_filename)
    image(dem, col = col_B61(20))
    lines(coord.parallel$x, coord.parallel$y,col = "red")
    points(initial.coord[1], initial.coord[2], cex = 0.7,col = "black", pch = 16)
    dev.off()

        
    print(coord.parallel)


}
}