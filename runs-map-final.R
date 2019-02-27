source("global.R")

library(dplyr)


make_runs_map <- function() {
  
  # The Mfg plates *actually run*
  mfg_plates <-
    get_mfg_table() %>%
    select(Day, Shift, Run, MfgPlate) %>%
    unique() %>%
    rename(ID=MfgPlate) %>%
    mutate(Class="Mfg")
  
  # MSA plates *actually run*
  msa_plates <-
    get_msa_table() %>%
    select(Day, Shift, Run, MSAPlate) %>%
    unique() %>%
    rename(ID=MSAPlate) %>%
    mutate(Class="MSA")
  
  # Get plates that were discarded during testing
  lost_plates <-
    get_lost_map() %>%
    select(Day, Shift, Run, ID) %>%
    mutate(Class="Lost")
  
  # Now merge these:
  runs_map <-
    rbind(mfg_plates, msa_plates, lost_plates) %>%
    arrange(Day, Shift, Run, ID)
    
  write.csv(runs_map, "../Plate maps/RunsMap.csv", row.names=FALSE)
}