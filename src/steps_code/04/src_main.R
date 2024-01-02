main.function_04 <- function(key, root_dir){

step_name = "04_terminus"

source(paste0(root_dir,"/config.R"))
source(paste0(root_dir, "/output/",key,"/04_terminus/glacier_list.R"))
source(paste0(root_dir, "/src/steps_code/04/functions.R"))
source(paste0(root_dir, "/src/base_functions.R"))
source(paste0(root_dir, "/input/", key,".R"))

library(fda); library(splines); library(mgcv); library(MASS); library(fields)
library(grDevices); library(scales); library(matrixStats)
library(phonTools)
library("customizedTraining")
library('matlab')

work_dir_path = config$work_dir_path
step_03_output_dir = paste0(root_dir, "/output/", key, "/03_process_landsat/output/")

distPerYear = params$step_4$distPerYear


output_dir = paste0(work_dir_path, "/output/",key,"/",step_name)
create_directory(output_dir, "output")
FV = get_FV_joined_file()

for(glacier in glacier_list){
    #TODO add to config
    meas = FV[which(FV$glac_id == glacier),8:7]

    #Get arc length in meters between each path point


    ts_intensity = readRDS(paste0(step_03_output_dir, glacier, "_ts_intensity.rds"))
    dates_cut = readRDS(paste0(step_03_output_dir, glacier, "_dates_cut.rds"))
    al = readRDS(paste0(step_03_output_dir, glacier, "_al.rds"))

    create_directory(output_dir, "output")
    create_directory(paste0(output_dir,"/output/"), "plots")
    create_directory(paste0(output_dir,"/output/plots"), glacier)
    plot_path = paste0(output_dir, "/output/plots/", glacier,"/")

    al = c(0,cumsum(rep(al[length(al)]/(ncol(ts_intensity)-1), (ncol(ts_intensity)-1))))
    term_paths = terminus(glacier, ts_intensity, al, dates_cut, plot = TRUE, distPerYear = distPerYear,linefit = 0,temporal = 1, direc = plot_path, meas = meas, knotbuffer = 2)

    #TODO structure outs better

    outs_filename = paste0(output_dir, "/output/", glacier,"_outs.rds")
    saveRDS(term_paths$outs, file = outs_filename)

    sSmooth_filename = paste0(output_dir, "/output/", glacier,"_sSmooth.rds")
    saveRDS(term_paths$sSmooth, file = sSmooth_filename)


}
}