# db.R
#
# Functions for building a SQLite database from the data and mappings files.
#
# This is for easy exploration of the data set---nothing more.  It isn't
# strictly needed for the application.


library(dbplyr)
library(dplyr)


delete_db <- function(filename = "xlot-data.sqlite") {

  if (file.exists(filename)) {
    res <- unlink(DATABASE, force = TRUE)

    if (res == 1L) {
      # You may have to force garbage collection if a connection is left
      # dangling...
      gc()
    }
  }

}


build_db <- function(filename = "xlot-data.sqlite", overwrite = FALSE) {

  if (!overwrite && file.exists(filename) ) {
    stop("Database file exists; will not overwrite.")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), filename)
  on.exit(DBI::dbDisconnect(con))

  # ----- The testing lot -----

  readings <- get_readings(factorize = FALSE)
  copy_to(con, readings, "Readings", temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "AssayRow", "AssayCol") )

  mfg_map <- collate_mfg_maps(factorize = FALSE)
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

  msa_runs_map <- get_msa_runs_map(factorize = FALSE)
  copy_to(con, msa_runs_map, "MSARunsMap",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Run") )


  # ----- The derived tables -----

  # Prefix each derived table with "Gen_" to distinguish them from the "raw"
  # tables.

  shift_order <- build_shift_order_table(readings, factorize = FALSE)
  copy_to(con, shift_order, "Gen_ShiftOrder",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift") )

  mfg_table <- build_mfg_table(readings, mfg_map, shift_order,
                               factorize = FALSE)
  copy_to(con, mfg_table, "Gen_MfgTable",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "WellOrder") )

  summary_table <- build_summary_table(mfg_table)
  copy_to(con, summary_table, "Gen_SummaryTable",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "MfgPlate") )


  msa_map <- build_msa_map(msa_mfg_map, msa_assembly_map)
  copy_to(con, msa_map, "Gen_MSAMap",
          temporary = FALSE, overwrite = overwrite,
          indices = c("MSAPlate", "AssayStrip") )

  msa_table <- build_msa_table(readings, msa_map, msa_runs_map,
                               factorize = FALSE)
  copy_to(con, msa_table, "Gen_MSATable",
          temporary = FALSE, overwrite = overwrite,
          indices = c("Day", "Shift", "Run", "WellOrder") )

}
