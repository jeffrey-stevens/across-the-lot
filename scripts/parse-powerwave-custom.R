# TODO: Add comment
# 
# Author: jstevens
###############################################################################


library(stringr)
library(reshape2)


parse_powerwave_file <- function(fname) {
	
	lines <- readLines(fname, skipNul=TRUE)
	
	# Plate number:  Line 7
	plate_number <- 
    as.integer(str_match(lines[[7]], "^Plate Number\tPlate (\\d+)")[1,2])
  
  # Single reads:
  linenos <- which(str_detect(lines, "^Results"))
  single_reads <-
    lapply(linenos, 
      function(ln) {
        data <- read.delim(fname, skip=ln+1, header=TRUE,
                           nrows=96)
        # Remove the "Well.ID" column:
        data <- data[,-1]
        # Break apart the Well column:
        wells <- data[,1]
        rowcols <- str_match(wells, "^([A-H])(\\d[012]?)$")[,-1]
        # Translate letters to numbers
        rows <- chartr("ABCDEFGH", "12345678", rowcols[,1])
        cols <- rowcols[,2]
        data <- cbind(AssayRow=rows, AssayCol=cols, data[,c(2,3)])

        # Get the read # (1 or 3)
        match <- str_match(colnames(data)[[3]], "^Read\\.(\\d+)\\.(450|650)$")
        readno <- as.integer(match[[2]])
        colnames(data)[3:4] <- c("A450", "A650")
        
        return(list(Read=readno, Data=data))
    })
  
  
  kinetic_reads <- 
    lapply(c(450, 650),
         function(wl) {
           read <- 2
           text <- paste0("^Read ", read, ":", wl)
           pd_line <- which(str_detect(lines, text))[[1]]
           times <- paste0(0:5, " min")
           data <- 
              read.delim(fname, skip=pd_line+3, 
                          nrows=96, header=FALSE)[,-8]

            wells <- data[,1]
            rowcols <- str_match(wells, "^([A-H])(\\d[012]?)$")[,-1]
            # Translate letters to numbers
            rows <- chartr("ABCDEFGH", "12345678", rowcols[,1])
            cols <- rowcols[,2]

            data_new <- cbind(rows, cols, data[,-1])
            colnames(data_new) <- c("AssayRow", "AssayCol", times)

           return(list(Wavelength=wl, Data=data_new))
         })

  return(list(PlateNo=plate_number, Single=single_reads, Kinetic=kinetic_reads))
}


get_matrix <- function(lines, read, wavelength) {
  txt <- paste0("^Read ", read, ":", wl)
  pd_line <- which(str_detect(lines, txt))[[1]]
  data <- 
    data.matrix(read.delim(fname, skip=pd_line, 
                           nrows=8, header=FALSE))
  
}



flatten_matrix <- function(mat) {
	
	flat_table <- melt(mat)
	names(flat_table) <- c("Row", "Column", "OD")
  flat_table <- flat_table[with(flat_table, order(Row, Column)),]
	
	return(flat_table)
}