# data-file.R
#
# Functions to read data and mapping files.


# Functions for loading raw data files ------------------------------------

get_readings <- function() {
  readr::read_csv( READINGS_FILE, col_names = TRUE,
                   col_types = cols(
                     Day = col_integer(),
                     Shift = col_character(),
                     Run = col_integer(),
                     AssayRow = col_integer(),
                     AssayCol = col_integer(),
                     A450 = col_double(),
                     A650 = col_double()
                   )
                  )
}



# The record of plates lost during testing
get_lost_map <- function() {
  readr::read_csv( LOST_PLATES_FILE, col_names = TRUE,
                   col_types = col( Day = col_integer(),
                                    Shift = col_character(),
                                    Run = col_integer(),
                                    Class = col_character(),
                                    ID = col_integer() )
                   )
}


# The record of plates lost during manufacture
get_discarded_map <- function() {
  readr::read_csv( DISCARDED_PLATES_FILE, col_names = TRUE,
                   col_types = col( MfgPlate = col_integer() )
                  )
}


get_runs_map <- function() {
  readr::read_csv( RUNS_MAP_FILE, col_names = TRUE,
                   col_types = col( Day = col_integer(),
                                    Shift = col_character(),
                                    Run = col_integer(),
                                    ID = col_integer(),
                                    Class = col_character() )
                  )
}


get_msa_map <- function() {
  readr::read_csv( MSA_MAP_FILE, col_names = TRUE,
                   col_types = col( MSAPlate = col_integer(),
                                    AssayStrip = col_integer(),
                                    MfgPlate = col_integer(),
                                    Pool = col_character(),
                                    MfgStrip = col_integer() )
                   )
}




# Functions for reading generated data files ------------------------------


get_mfg_table <- function() {
  
  mfg_table <- read.csv(MFG_MASTER_FILE)
  mfg_table[["Well"]] <- 
    ordered(mfg_table[["Well"]], levels=get_wells()$Well)
  mfg_table[["Shift"]] <-
    ordered( mfg_table[["Shift"]], levels=c("Day", "Evening") )
  
  return(mfg_table)
}


get_mfg_summary <- function() {
  summary_tab <- 
    read.csv(MFG_SUMMARY_FILE)
}


get_msa_table <- function() {
  read.csv(MSA_MASTER_FILE)
}