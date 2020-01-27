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

get_data_dir <- function() {
  system.file("extdata", package = "xlot", mustWork = TRUE)
}

# The location of all intermediate data tables
get_raw_data_dir <- function() {
  file.path(get_data_dir(), "raw-data")
}

# The location of all "mapping" files (plate maps, etc.;
# basically readings metadata.)
get_maps_dir <- function() {
  file.path(get_data_dir(), "maps")
}

# Directory of generated data and mapping files
get_generated_dir <- function() {
  file.path(get_data_dir(), "generated")
}


# ----- Raw data and mapping files -----

# The file containing all the raw (unmapped) OD readings
get_readings_file <- function() {
  file.path(get_raw_data_dir(), "Readings.csv")
}

# The record of plates lost during the testing runs
get_lost_plates_file <- function() {
  file.path(get_raw_data_dir(), "LostPlates.csv")
}

# The record of plates discarded during the manufacturing run
get_discarded_plates_file <- function() {
  file.path(get_raw_data_dir(), "DiscardedPlates.csv")
}

# Map of "Pool" plates to the plate's manufacturing number
get_msa_mfg_file <- function() {
  file.path(get_maps_dir(), "MSAMfgMap.csv")
}

# Map of how the MSA plates were assembled
get_msa_assembly_file <- function() {
  file.path( get_maps_dir(), "MSAAssemblyMap.csv")
}

get_msa_runs_file <- function() {
  file.path( get_maps_dir(), "MSARunsMap.csv")
}


# The "Runs randomization" directory
get_runs_rand_dir <- function() {
  file.path( get_maps_dir(), "Runs Randomization")
}



# Generated data files ----------------------------------------------------


# The record of the runs (day, shift, run-in-shift, plate ID)
# Not sure if this should be considered generated...
get_runs_map_file <- function() {
  file.path( get_generated_dir(), "RunsMap.csv" )
}


# Static assets

get_description_file <- function() {
  system.file("extdata", "description.md", package = "xlot")
}



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
