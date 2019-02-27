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

# Directory of generated data and mapping files
GENERATED_DIR <- file.path(DATA_DIR, "generated")


# ----- Raw data and mapping files -----

# The file containing all the raw (unmapped) OD readings
READINGS_FILE <- file.path(DATA_TABLES_DIR, "Readings.csv")

# The record of plates lost during the testing runs
LOST_PLATES_FILE <- file.path( MAPS_DIR, "LostPlates.csv")

# The record of plates discarded during the manufacturing run
DISCARDED_PLATES_FILE <- file.path( MAPS_DIR, "DiscardedPlates.csv")

# The record of the runs (day, shift, run-in-shift, plate ID)
RUNS_MAP_FILE <- file.path( MAPS_DIR, "RunsMap.csv" )

# The "Runs randomization" directory
RUNS_RAND_DIR <- file.path( MAPS_DIR, "Runs Randomization")


# ----- Generated data and mapping files -----

# The file of merged maps and OD readings for the "Manufacturing"
# (non-MSA) plates
MFG_MASTER_FILE <- file.path(DATA_TABLES_DIR, "MfgMastertable.csv")

# A file of summary data for the MfgMastertable
MFG_SUMMARY_FILE <- file.path(DATA_TABLES_DIR, "MfgSummary.csv")

# The mapping data for the embedded MSA
MSA_MAP_FILE <- file.path(MAPS_DIR, "MSAMap.csv") 

# The table of mapped MSA data
MSA_MASTER_FILE <- file.path(DATA_TABLES_DIR, "MSAMastertable.csv") 

# The master SQLite database
DATABASE <- file.path( GENERATED_DIR, "alldata.sqlite")


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