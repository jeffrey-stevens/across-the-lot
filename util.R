source("global.R")

library(dplyr)


# Add well names, run order to the table
expand_tab <- function(mfg_table) {
  
  run_order <- read.csv("../Plate maps/ShiftOrdering.csv")
  
  well_names <- 
    expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
    mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                              paste0(LETTERS[1:8], collapse=""),
                              AssayRow),
                       AssayCol),
           WellOrder=as.numeric(seq_len(n())))
  # Well ordering is right!
  # May want to store this...
  
  # Now merge with the data table:
  data_tab <- 
    mfg_table %>%
    inner_join(well_names) %>%
    inner_join(run_order)

  return(data_tab)
}


calc_summary <- function(data_tab) {

  summary_tab <-
    data_tab %>%
    group_by(Day, Shift, Run, MfgPlate) %>%
    summarise(Mean450=mean(A450), SD450=sd(A450), CV450=sd(A450)/mean(A450)*100,
              Median450=median(A450), Range450=diff(range(A450)),
              RelRange450=diff(range(A450))/mean(A450)*100,
              Mean650=mean(A650), SD650=sd(A650), CV650=sd(A650)/mean(A650)*100,
              Median650=median(A650), Rang650=diff(range(A650)),
              RelRange650=diff(range(A650))/mean(A650)*100) %>%
    arrange(MfgPlate)
    
  return(summary_tab)
}