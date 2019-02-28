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
  on.exit(DBI::dbDisconnect(con))
  
  # ----- The testing lot -----
  
  readings <- get_readings()
  copy_to(con, readings, "Readings", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "AssayRow", "AssayCol") )
  
  mfg_map <- collate_mfg_maps()
  copy_to(con, mfg_map, "MfgMap", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "MfgPlate") )
  
  lost_map <- get_lost_map()
  copy_to(con, lost_map, "LostMap", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run") )
  
  discarded_map <- get_discarded_map()
  copy_to(con, discarded_map, "DiscardedMap", temporary = FALSE,
          overwrite = overwrite)
  
  # ----- The MSA pool -----
  
  msa_mfg_map <- get_msa_mfg_map()
  copy_to(con, msa_mfg_map, "MSAMfgMap",
          temporary = FALSE, overwrite = overwrite,
          indices = "PoolPlateID" )
  
  msa_assembly_map <- get_msa_assembly_map()
  copy_to(con, msa_assembly_map, "MSAAssemblyMap",
          temporary = FALSE, overwrite = overwrite,
          indices = c("MSAPlate", "AssayStrip") )
  
  msa_runs_map <- get_msa_runs_map()
  copy_to(con, msa_runs_map, "MSARunsMap",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Run") )
  
}
