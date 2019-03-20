check_mfg <- function(mfg_table) {

  # Get the number of plates run

}


mfg_plates_run <- function(mfg_table) {
  length(unique(mfg_table$MfgPlate))
}



# Get missing plates

get_missing <- function(mfg_table) {
  plates <- unique(mfg_table$MfgPlate)

  absent <- setdiff(seq_len(RUN_SIZE), plates)

  return(absent)
}


find_dupes <- function(mfg_table) {
  # Count # of entries for each Mfg plate:
  tab <- table(mfg_table$MfgPlate)
  dupes <- which(tab != 96 )
  tab[dupes]
}


# Okay...319 was recorded twice...When were they run?

get_runs <- function(mfg_table, mfg_plate) {
  subtab <- subset(mfg_table,
                MfgPlate == mfg_plate)[
                  , c("MfgPlate", "Day", "Shift", "Run")]
  subtab2 <- unique(subtab)
  return(subtab2)
}


count_data_files <- function() {
  library(dplyr)

  files <- list.files("../Data files/", recursive=TRUE)

  pat <- "^Day (\\d{1,2})/(Day|Evening)/(.*.txt)$"
  mat <- stringr::str_match(files, pat)
  files_frame <-
    data.frame(Day=as.integer(mat[,2]),
               Shift=mat[,3],
               File=mat[,4])

  summary_frame <-
    files_frame %>%
    group_by(Day, Shift) %>%
    summarise(Count=length(File)) %>%
    arrange(Day, Shift)

  return(as.data.frame(summary_frame))
}


count_msa_plates <- function() {
  msa_run <- read.csv("../Data tables/MSAMastertable.csv")
  msa_plates <- sort(unique(msa_run$MSAPlate))

  cat("\n", "MSA plates run:\n")
  print(msa_plates)
  cat("\n")

  n <- length(msa_plates)
  cat("# of MSA plates run:  ", n, "\n\n")

  # Expected # of msa plates
  msa_map <- read.csv("../Plate maps/MSARunsMap.csv")
  map_n <- length(msa_map$MSAPlate)
  cat("Expected # of MSA plates:  ", map_n, "\n\n")

}
