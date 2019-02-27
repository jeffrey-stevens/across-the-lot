library(stringr)

rand_dir <- file.path(MAPS_DIR, "Runs randomization")
map_files <- list.files(rand_dir)

mfg_maps <- 
  lapply(map_files, 
         function(f) {
           # Parse f to get the Day & Shift:
           m <- str_match(f, 
                  "^Day (\\d+), (Day|Evening) shift\\.csv$")[1,2:3]
           day <- m[[1]]
           shift <- m[[2]]
           
           full_path <- file.path(rand_dir, f)
           map <- read.csv(full_path)

           return(cbind(Day=day, Shift=shift, map))
         })

mfg_map_1 <- do.call(rbind, mfg_maps)
mfg_map_2 <- na.omit(mfg_map_1)

outfile <- file.path(MAPS_DIR, "MfgMap.csv")
write.csv(mfg_map_2, outfile, row.names=FALSE)