# read-data.R
#
# Functions to read data and mapping files.


#' @importFrom readr col_integer col_character col_double
NULL


# ----- Raw data files for the test ("Mfg") lot -----

get_readings <- function(factorize = TRUE) {
  # factorize:  Shift to an ordered factor?

  tab <- readr::read_csv( get_readings_file(), col_names = TRUE,
                          col_types = readr::cols(
                            Day = col_integer(),
                            Shift = col_character(),
                            Run = col_integer(),
                            AssayRow = col_integer(),
                            AssayCol = col_integer(),
                            A450 = col_double(),
                            A650 = col_double()
                          ) )

  if (factorize) {
    tab <-
      tab %>%
      mutate(Shift = ordered(Shift, levels = c("Day", "Evening") ))
  }

  return(tab)
}


get_runs_map <- function(factorize = TRUE) {

  tab <- readr::read_csv( get_runs_map_file(), col_names = TRUE,
                          col_types = readr::cols(
                            Day = col_integer(),
                            Shift = col_character(),
                            Run = col_integer(),
                            ID = col_integer(),
                            Class = col_character() )
  )

  if (factorize) {
    tab <-
      tab %>%
      mutate(Shift = ordered(Shift, levels = c("Day", "Evening") ))
  }

  return(tab)
}


# The record of plates lost during testing
get_lost_map <- function(factorize = TRUE) {

  tab <- readr::read_csv( get_lost_plates_file(), col_names = TRUE,
                          col_types = readr::cols(
                            Day = col_integer(),
                            Shift = col_character(),
                            Run = col_integer(),
                            Class = col_character(),
                            ID = col_integer() ) ) %>%
    ## Class and ID are redundant!!!  Only the day, shift and run
    ## are needed here!
    select(Day, Shift, Run) %>%
    arrange(Day, Shift, Run)

  if (factorize) {
    tab <-
      tab %>%
      mutate(Shift = ordered(Shift, levels = c("Day", "Evening") ))
  }

  return(tab)
}


# The record of plates lost during manufacture
get_discarded_map <- function() {
  readr::read_csv( get_discarded_plates_file(), col_names = TRUE,
                   col_types = readr::cols( MfgPlate = col_integer() ) )
}


# Load a *single* mfg map file
load_mfg_map <- function(filename) {
  readr::read_csv( filename, col_names = TRUE,
                   col_types = readr::cols(
                     Run = col_integer(),
                     MfgPlate = col_integer() ) )
}


# Collate all individual (by-shift) "Runs randomization" files.
collate_mfg_maps <- function(rand_dir = get_runs_rand_dir(), factorize = TRUE) {

  map_files <- list.files(rand_dir)

  mfg_maps_list <-
    lapply(map_files,
           function(f) {
             # Parse f to get the Day & Shift:
             m <- stringr::str_match(f,
                    "^Day (\\d+), (Day|Evening) shift\\.csv$")[1,2:3]
             day <- as.integer(m[[1]])
             shift <- m[[2]]

             full_path <- file.path(rand_dir, f)
             map <-
               load_mfg_map(full_path) %>%
               mutate(Day = day, Shift = shift)

             return(map)
           })

  mfg_map <-
    mfg_maps_list %>%
    bind_rows() %>%
    na.omit() %>%
    select(Day, Shift, Run, MfgPlate) %>%
    arrange(Day, Shift, Run)


  if (factorize) {
    mfg_map <-
      mfg_map %>%
      mutate(Shift = ordered(Shift, levels = c("Day", "Evening") ))
  }

  return(mfg_map)
}



# ----- The MSA pool -----

get_msa_mfg_map <- function() {
  readr::read_csv( get_msa_mfg_file(), col_names = TRUE,
                   col_types = readr::cols( PoolPlateID = col_integer(),
                                            Pool = col_character(),
                                            MfgPlate = col_integer() ) )
}


get_msa_assembly_map <- function() {
  readr::read_csv( get_msa_assembly_file(), col_names = TRUE,
                   col_types = readr::cols( PoolPlateID = col_integer(),
                                            PoolPlateStrip = col_integer(),
                                            MSAPlate = col_integer(),
                                            AssayStrip = col_integer() ) ) %>%
    select(MSAPlate, AssayStrip, PoolPlateID, PoolPlateStrip) %>%
    arrange(MSAPlate, AssayStrip)
}


get_msa_runs_map <- function(factorize = TRUE) {
  tab <- readr::read_csv( get_msa_runs_file(), col_names = TRUE,
                          col_types = readr::cols( Day = col_integer(),
                                                   Shift = col_character(),
                                                   Run = col_integer(),
                                                   MSAPlate = col_integer() ) )
  ## This is somewhat messy...Really you should have 2 tables:  Day shift MSA
  ## plates and Evening shift MSA plates.


  if (factorize) {
    tab <-
      tab %>%
      mutate(Shift = ordered(Shift, levels = c("Day", "Evening") ))
  }

  return(tab)
}
