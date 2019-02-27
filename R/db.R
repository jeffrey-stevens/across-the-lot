# db.R
#
# Functions for building the SQLite data and mappings database.


source("R/global.R")
source("R/data-file.R")
source("R/mfg-map.R")

library(dbplyr)
library(dplyr)


build_db <- function(filename = DATABASE, overwrite = FALSE) {

  if (!overwrite && file.exists(filename) ) {
    stop("Database file exists; will not overwrite.")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), filename)
  
  readings <- get_readings()
  copy_to(con, readings, "Readings", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "AssayRow", "AssayCol") )
  
  mfg_map <- collate_mfg_maps()
  copy_to(con, mfg_map, "MfgMap", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "MfgPlate") )
  
  lost_map <- get_lost_map()
  copy_to(con, lost_map, "LostMap", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run") )
  
  discarded_map <- get_discarded()
  copy_to(con, discarded_map, "DiscardedMap", temporary = FALSE,
          overwrite = overwrite)
  
  
  DBI::dbDisconnect(con)

}
