
# ----- Table "getter" functions -----

get_readings_tbl <- function(con) {
  tbl(con, "Readings")
}

get_mfg_map_tbl <- function(con) {
  tbl(con, "MfgMap")
}

get_lost_map_tbl <- function(con) {
  tbl(con, "LostMap")
}

get_discarded_map_tbl <- function(con) {
  tbl(con, "DiscardedMap")
}

get_msa_mfg_tbl <- function(con) {
  tbl(con, "MSAMfgMap")
}

get_msa_assembly_tbl <- function(con) {
  tbl(con, "MSAAssemblyMap")
}

get_msa_runs_tbl <- function(con) {
  tbl(con, "MSARunsMap")
}
