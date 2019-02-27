# TODO: Add comment
# 
# Author: jstevens
###############################################################################


library(stringr)
library(plyr)

source("scripts/parse-powerwave-2.R")

ROOTDIR <- ".."
DATADIR <- file.path(ROOTDIR, "Data files")


get_all_data <- function(dir) {
  
  data_files <- list.files(dir, pattern=".*\\.txt$")
  
  all_data <- 
    lapply(data_files,
       function(f) {
         data_list <- parse_powerwave_file(file.path(dir, f))
         
         return(data_list)
       })
  
  return(all_data)
  
}


process_data <- function() {
  data_folders <- list.dirs(DATADIR, full.names=FALSE, recursive=FALSE)
  
  all_data <- 
    plyr::ldply(data_folders,
           function(f) {
             day <- as.integer(stringr::str_match(f, "^Day (\\d+)$")[1,2])
             
             plyr::ldply(c("Day", "Evening"),
                    function(shift) {
                      dir_path <- file.path(DATADIR, f, shift)
                      data_list <- get_all_data(dir_path)
                      # Add the day, the shift, the run
                      plyr::ldply(data_list,
                             function(d) {
                               run <- d$PlateNo
                               data <- d$Data
                               cbind(Day=day, Shift=shift, Run=run, data)
                             })
                    })
           })
  
  write.csv(all_data, "../Data tables/Readings.csv", row.names=FALSE)
}