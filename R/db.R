# db.R

library(dbplyr)

source("global.R")


# build_db <- function(filename = DATABASE, overwrite = FALSE) {
#   
#   if (!overwrite && file.exists(filename) ) {
#     stop("Database file exists; will not overwrite.")
#   }
#   
#   con <- DBI::connect(RSQLite::SQLite(), filename)
#   
# }
