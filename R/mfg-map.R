# mfg-map.R
#
# Collate all the individual "Runs randomization" files


collate_mfg_maps <- function(outfile = NULL) {
  
  map_files <- list.files(RUNS_RANDOMIZATION)
  
  mfg_maps_list <- 
    lapply(map_files, 
           function(f) {
             # Parse f to get the Day & Shift:
             m <- stringr::str_match(f, 
                            "^Day (\\d+), (Day|Evening) shift\\.csv$")[1,2:3]
             day <- m[[1]]
             shift <- m[[2]]
             
             full_path <- file.path(rand_dir, f)
             map <- 
               load_mfg_map(full_path) %>%
               mutate(Day = day, Shift = shift)
             
             return(map)
           })
  
  mfg_map <- 
    mfg_maps_list %>%
    bind_rows() %>%
    na.omit()
  
  if (!is.null(outfile)) {
    write.csv(mfg_map, outfile, row.names=FALSE)
  }
  
  
  return(mfg_map)
}

