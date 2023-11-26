    calculate_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    get_joined_file <- function(x) {
      work_dir_path = config$work_dir_path
      joined_csv_path = paste0(work_dir_path,"/bin/joined.csv")
      glaciers <- read.csv(joined_csv_path, header = T)
      return(glaciers)
    }

    