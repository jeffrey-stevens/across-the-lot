# global.R
#
# Various global constants, paths and data "getter" functions.



# Global constants --------------------------------------------------------

# The minimum and maximum plate numbers.
# Plate #1 was lost in the production run, so another plate was coated.
MFG_MIN <- 2
MFG_MAX <- 601



# Paths -------------------------------------------------------------------

# ----- Directories -----

# This is to get around hard-coded paths, probably due to the fact
# that Shiny would change the working directory.
# I should find a way of removing hard-path dependencies...
ROOT_DIR <- rprojroot::find_rstudio_root_file()
message("Implementing a temporary work-around for hard-path dependencies.")

DATA_DIR <- file.path(ROOT_DIR, "data")

# The location of all intermediate data tables
DATA_TABLES_DIR <- file.path(DATA_DIR, "data-tables")

# The location of all "mapping" files (plate maps, etc.;
# basically readings metadata.)
MAPS_DIR <- file.path(DATA_DIR, "maps")


# ----- Files -----

# The file containing all the raw (unmapped) OD readings
READINGS_FILE <- file.path(DATA_TABLES_DIR, "Readings.csv")

# The file of merged maps and OD readings for the "Manufacturing"
# (non-MSA) plates
MFG_MASTER_FILE <- file.path(DATA_TABLES_DIR, "MfgMastertable.csv")

# A file of summary data for the MfgMastertable
MFG_SUMMARY_FILE <- file.path(DATA_TABLES_DIR, "MfgSummary.csv")

# The mapping data for the embedded MSA
MSA_MAP_FILE <- file.path(MAPS_DIR, "MSAMap.csv") 

# The table of mapped MSA data
MSA_MASTER_FILE <- file.path(DATA_TABLES_DIR, "MSAMastertable.csv") 

# The record of plates lost during the testing runs
LOST_PLATES_FILE <- file.path( MAPS_DIR, "LostPlates.csv")

# The record of plates discarded during the manufacturing run
DISCARDED_PLATES_FILE <- file.path( MAPS_DIR, "DiscardedPlates.csv")

# The record of the runs (day, shift, run-in-shift, plate ID)
RUNS_MAP_FILE <- file.path( MAPS_DIR, "RunsMap.csv" )



# Utility functions -------------------------------------------------------

get_wells <- function() {
  # dplyr conflicts with plyr:
  if ( "package:plyr" %in% search() ) {
    detach("package:plyr")
  }
  library(dplyr)
  
  well_names <- 
    expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
    mutate( Well = 
              paste0(
                chartr(paste0(1:8, collapse=""),
                       paste0(LETTERS[1:8], collapse=""),
                       AssayRow),
                AssayCol),
            WellOrder=as.numeric(seq_len(n()))) %>%
    mutate(Well=ordered(Well, levels=Well))  # Sort it...
  
  return(well_names)
}



# File-loading functions --------------------------------------------------


get_readings <- function() {
  read.csv(READINGS_FILE)
}


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


get_msa_map <- function() {
  read.csv(MSA_MAP_FILE)
}


get_msa_table <- function() {
  read.csv(MSA_MASTER_FILE)
}


# The record of plates lost during testing
get_lost_map <- function() {
  read.csv(LOST_PLATES_FILE)
}


# The record of plates lost during manufacture
get_discarded_map <- function() {
  read.csv(DISCARDED_PLATES_FILE)
}


get_runs_map <- function() {
  read.csv(RUNS_MAP_FILE)
}