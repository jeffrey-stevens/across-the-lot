# data-file.R
#
# Functions to read data and mapping files.


source("R/global.R")

library(readr)
library(dplyr)


# Functions for loading raw data files ------------------------------------


# ----- Raw data files for the test lot -----

get_readings <- function() {
  read_csv( READINGS_FILE, col_names = TRUE,
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


get_runs_map <- function() {
  read_csv( RUNS_MAP_FILE, col_names = TRUE,
                   col_types = cols( 
                     Day = col_integer(),
                     Shift = col_character(),
                     Run = col_integer(),
                     ID = col_integer(),
                     Class = col_character() )
                  )
}


# The record of plates lost during testing
get_lost_map <- function() {
  
  read_csv( LOST_PLATES_FILE, col_names = TRUE,
                   col_types = cols( 
                     Day = col_integer(),
                     Shift = col_character(),
                     Run = col_integer(),
                     Class = col_character(),
                     ID = col_integer() )
                   ) %>%
    ## Class and ID are redundant!!!  Only the day, shift and run
    ## are needed here!
    select(Day, Shift, Run) %>%
    arrange(Day, Shift, Run)
  
}


# The record of plates lost during manufacture
get_discarded_map <- function() {
  read_csv( DISCARDED_PLATES_FILE, col_names = TRUE,
                   col_types = cols( MfgPlate = col_integer() ) )
}


# Load a *single* mfg map file
load_mfg_map <- function(filename) {
  read_csv( filename, col_names = TRUE,
                   col_types = cols( 
                     Run = col_integer(),
                     MfgPlate = col_integer() ) )
}


# ----- The MSA pool -----

get_msa_mfg_map <- function() {
  read_csv( MSA_MFG_FILE, col_names = TRUE,
            col_types = cols( PoolPlateID = col_integer(),
                              Pool = col_character(),
                              MfgPlate = col_integer() ) )
}


get_msa_assembly_map <- function() {
  read_csv( MSA_ASSEMBLY_FILE, col_names = TRUE,
            col_types = cols( PoolPlateID = col_integer(),
                              PoolPlateStrip = col_integer(),
                              MSAPlate = col_integer(),
                              AssayStrip = col_integer() ) ) %>%
    select(MSAPlate, AssayStrip, PoolPlateID, PoolPlateStrip) %>%
    arrange(MSAPlate, AssayStrip)
}


get_msa_runs_map <- function() {
  read_csv( MSA_RUNS_FILE, col_names = TRUE,
            col_types = cols( Day = col_integer(),
                              Shift = col_character(),
                              Run = col_integer(),
                              MSAPlate = col_integer() ) )
  ## This is somewhat messy...Really you should have 2 tables:  Day shift MSA
  ## plates and Evening shift MSA plates.
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


## Eventually get rid of these...

get_mfg_summary <- function() {
  summary_tab <- 
    read.csv(MFG_SUMMARY_FILE)
}


get_msa_map <- function() {
  read_csv( MSA_MAP_FILE, col_names = TRUE,
                   col_types = cols( 
                     MSAPlate = col_integer(),
                     AssayStrip = col_integer(),
                     MfgPlate = col_integer(),
                     Pool = col_character(),
                     MfgStrip = col_integer() )
                   )
}


get_msa_table <- function() {
  read.csv(MSA_MASTER_FILE)
}