col_list = c('black','red','yellow','green', 'blue','pink','brown', 'purple', 'cyan', 'magenta', 'grey', 'darkgreen', 'darkblue', 'lightblue', 'magenta','magenta','magenta','magenta')

plot_rgb <- function(glacier, landsatImg, initial.coord, coord.parallel, term_paths, al, index) {
  plotRGB(landsatImg, r = 3, g = 2, b = 1, stretch = "hist", axes = T,
          main = paste(as.character(names(landsatImg)[1])))

  lines(coord.parallel$x, coord.parallel$y, col = "red", lwd = 1.5)
  points(initial.coord[1], initial.coord[2], cex = 1.2, col = "black", pch = 13)

  legend_labels <- c("Start")
  legend_colors <- c("black")
  col_list <- c("red", "blue", "green", "purple") # Add appropriate colors here

  for (i in seq_along(term_paths)) {
    ind = term_paths[[i]][index]
    points(coord.parallel$x[ind], coord.parallel$y[ind], col = col_list[i], cex = 1.5, pch = 16)
    legend_labels <- c(legend_labels, paste("Path", i))
    legend_colors <- c(legend_colors, col_list[i])
  }
  legend("bottomright", legend = legend_labels, col = legend_colors, pch = c(13, rep(16, length(term_paths))))
}


  animate_rgb <- function(glacier, landsatImgs, initial.coord, coord.parallel, term_paths,al ,plot_path){
  library(raster) #Needed for image processing
  library("rgdal") #Needed for image processing
  library(animation) # contains the savGIF function
  require(devtools)

  print("Generating frames")
  pb <- txtProgressBar(min = 0, max = length(landsatImgs), style = 3)
    saveGIF(for(i in c(1:length(landsatImgs))){
          setTxtProgressBar(pb, i)
    plot_rgb(glacier, landsatImgs[[i]], initial.coord, coord.parallel ,term_paths,al, index = i)
    },height = 500, width = 350, interval = 0.1, nmax = 500, movie.name = plot_path)  
    close(pb)

  print("Done making the GIF")
}