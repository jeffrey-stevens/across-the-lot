source("R/global.R")

library(dplyr)


# Determine the shift ordering from the Readings file
shift_ordering_map <- function( readings = NULL, save = TRUE ) {
  
  if ( is.null(readings) ) {
    readings <- get_readings()
  }
  
  shift_order <-
    readings %>%
    select(Day, Shift) %>%
    unique() %>%
    arrange(Day, Shift) %>%
    mutate(RunOrder=seq_len(n()))
  
  # Now save:
  if ( save ) {
    write.csv(shift_order, RUNS_ORDERING_FILE, row.names=FALSE)
  }
  
  return(shift_order)
}