# TODO: Add comment
# 
# Author: jstevens
###############################################################################


library(stringr)
library(reshape2)


parse_powerwave_file <- function(fname) {
	
	lines <- readLines(fname, skipNul=TRUE)
	
	# Plate number:  Line 3
	plate_number <- 
    as.integer(str_match(lines[[3]], "^Plate Number:\tPlate (\\d+)\t\t$")[1,2])
  
  # Barcode:  Line 5
	barcode <- str_match(lines[[5]], "^Barcode:\t(.*)\t\t$")[1,2]
  
  # Omitting other captured data for now...
  
  ## Plate data:
  
  # Capture the "Plate Data" line:
  pd_line <- which(str_detect(lines, "Plate Data"))[[1]]
  
  # Capture the data
	data <- data.matrix(read.delim(fname, skip=pd_line, header=FALSE))
  colnames(data) <- 1:12
  # Should be able to read to the end of the file
	
	return(
    list(
			platenumber=plate_number,
      barcode=barcode,
			data=data
		)
	)
  
}



flatten_matrix <- function(mat) {
	
	flat_table <- melt(mat)
	names(flat_table) <- c("Row", "Column", "OD")
  flat_table <- flat_table[with(flat_table, order(Row, Column)),]
	
	return(flat_table)
}