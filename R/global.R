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

DATA_DIR <- system.file("extdata", package = "xlot", mustWork = TRUE)

# The location of all intermediate data tables
RAW_DATA_DIR <- file.path(DATA_DIR, "raw-data")

# The location of all "mapping" files (plate maps, etc.;
# basically readings metadata.)
MAPS_DIR <- file.path(DATA_DIR, "maps")

# Directory of generated data and mapping files
GENERATED_DIR <- file.path(DATA_DIR, "generated")


# ----- Raw data and mapping files -----

# The file containing all the raw (unmapped) OD readings
READINGS_FILE <- file.path(RAW_DATA_DIR, "Readings.csv")

# The record of plates lost during the testing runs
LOST_PLATES_FILE <- file.path( RAW_DATA_DIR, "LostPlates.csv")

# The record of plates discarded during the manufacturing run
DISCARDED_PLATES_FILE <- file.path( RAW_DATA_DIR, "DiscardedPlates.csv")

# Map of "Pool" plates to the plate's manufacturing number
MSA_MFG_FILE <- file.path( MAPS_DIR, "MSAMfgMap.csv")

# Map of how the MSA plates were assembled
MSA_ASSEMBLY_FILE <- file.path( MAPS_DIR, "MSAAssemblyMap.csv")

MSA_RUNS_FILE <- file.path( MAPS_DIR, "MSARunsMap.csv")


# The "Runs randomization" directory
RUNS_RAND_DIR <- file.path( MAPS_DIR, "Runs Randomization")



# Utility functions -------------------------------------------------------

get_wells <- function(factorize = TRUE) {
  # Convert "Well" to an ordered factor?

  well_names <-
    expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
    mutate( Well =
              paste0(
                chartr(paste0(1:8, collapse=""),
                       paste0(LETTERS[1:8], collapse=""),
                       AssayRow),
                AssayCol),
            WellOrder=as.numeric(seq_len(n())))

  if (factorize) {
    well_names <-
      well_names %>%
      mutate( Well = ordered(Well, levels=Well) )  # Sort it...
  }

  return(well_names)
}
