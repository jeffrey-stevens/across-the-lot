

options(shiny.trace=FALSE)


# Constants ---------------------------------------------------------------

SHINY_PORT <- 5109


# Functions ---------------------------------------------------------------


# Build all the MSA tables
#
# This helps save memory by GC'ing the intermediate tables...
build_msa <- function(readings) {

  msa_mfg <- get_msa_mfg_map()
  msa_assembly <- get_msa_assembly_map()
  msa_map <- build_msa_map(msa_mfg, msa_assembly)

  msa_runs <- get_msa_runs_map(factorize = TRUE)
  msa_table <- build_msa_table(readings, msa_map, msa_runs, factorize = TRUE)

  return(msa_table)
}


# Build all Mfg tables
build_mfg <- function(readings) {

  shift_order <- build_shift_order_table(readings)
  mfg_map <- collate_mfg_maps(factorize = TRUE)

  mfg_table <- build_mfg_table(readings, mfg_map, shift_order, factorize = TRUE)

  return(mfg_table)
}


#' run_shiny
#'
#' @param debug Not yet implemented.
#'
#' @return
#' @export
#'
#' @examples
run_shiny <- function(debug = FALSE) {

  if (debug) {
    warning("Shiny debugging isn't implemented yet...")
  }

  # Store the tables in-memory, for faster access
  readings <- get_readings(factorize = TRUE)
  runs_map <- get_runs_map(factorize = TRUE)
  msa_table <- build_msa(readings)
  mfg_table <- build_mfg(readings)
  summary_table <- build_summary_table(mfg_table)

  days <- max(readings$Day)
  mfg_min <- min(mfg_table$MfgPlate)
  mfg_max <- max(mfg_table$MfgPlate)
  ymax450 <- max(mfg_table$A450)
  ymax650 <- max(mfg_table$A650)


  # Create the parameter list for Shiny, and run the app
  ui <- build_ui( no.days = days, mfg.min = mfg_min, mfg.max = mfg_max,
                  ymax450 = ymax450, ymax650 = ymax650 )
  server <- build_server(readings, runs_map, msa_table, mfg_table,
                         summary_table)

  runApp( list(ui = ui, server = server), port = SHINY_PORT,
          launch.browser = interactive() )
}

