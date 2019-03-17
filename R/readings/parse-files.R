# parse-files.R
#
# For collating the raw OD output files from the Biotek Powerwave reader. I no
# longer have access to the raw Powerwave files, but I'll keep this code to
# preserve the whole processing pipeline.



# Parse and collect all the raw PowerWave files of a shift into a single list,
# for collation.

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


# Collate all the individual (run-by-run) Powerwave tables.

process_data <- function(datadir, outfile = READINGS_FILE) {
  data_folders <- list.dirs(datadir, full.names=FALSE, recursive=FALSE)

  all_data <-
    plyr::ldply(data_folders,
           function(f) {
             day <- as.integer(stringr::str_match(f, "^Day (\\d+)$")[1,2])

             plyr::ldply(c("Day", "Evening"),
                    function(shift) {
                      dir_path <- file.path(datadir, f, shift)
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

  write.csv(all_data, outfile, row.names=FALSE)
}
