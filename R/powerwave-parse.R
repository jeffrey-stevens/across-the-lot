# parse-powerwave.R
#
# This is a simple parser for raw Biotek Powerwave / Gen5 OD export files.
# Though I don't have access to the original files, I'll keep this to preserve
# the entire data processing pipeline.
###############################################################################


# This will parse PowerWave export files in "long" format

parse_powerwave_file <- function(fname) {

	lines <- readLines(fname, skipNul=TRUE)

	# Plate number:  Line 7
	plate_number <-
    as.integer(stringr::str_match(lines[[7]], "^Plate Number\tPlate (\\d+)")[1,2])

  # Get the table
  lineno <- which(stringr::str_detect(lines, "^Results"))[[1]]
  data <- read.delim(fname, skip=lineno+1, header=TRUE,
                     nrows=96)
  # Remove the "Well.ID" column:
  data <- data[,-1]
  # Break apart the Well column:
  wells <- data[,1]
  rowcols <- stringr::str_match(wells, "^([A-H])(\\d[012]?)$")[,-1]
  # Translate letters to numbers
  rows <- chartr("ABCDEFGH", "12345678", rowcols[,1])
  cols <- rowcols[,2]
  data <- cbind(AssayRow=rows, AssayCol=cols, data[,c(2,3)])

  colnames(data)[3:4] <- c("A450", "A650")


  return(list(PlateNo=plate_number, Data=data))
}
