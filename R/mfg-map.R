library(stringr)

map_files <- list.files("../Plate maps/Runs randomization")

mfg_maps <- 
  lapply(map_files, 
         function(f) {
           # Parse f to get the Day & Shift:
           m <- str_match(f, 
                  "^Day (\\d+), (Day|Evening) shift\\.csv$")[1,2:3]
           day <- m[[1]]
           shift <- m[[2]]
           
           full_path <- file.path("../Plate maps/Runs randomization", f)
           map <- read.csv(full_path)

           return(cbind(Day=day, Shift=shift, map))
         })

mfg_map_1 <- do.call(rbind, mfg_maps)
mfg_map_2 <- na.omit(mfg_map_1)

write.csv(mfg_map_2, "../Plate maps/MfgMap.csv", row.names=FALSE)