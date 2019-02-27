source("assay-plate-pic.R")



show_mfg_plate <- function(mfg_table, plate, minod=NULL, maxod=NULL, wl=450L,
                           palette=NULL) {
  #  plate:  A plate number
  
  sub <- subset(mfg_table, MfgPlate==plate)
  n <- nrow(sub)
  if (identical(n, 0L)) {
    warning(paste("Mfg plate", plate, "not found."))
  } else if (n < 96L) {
    warning(paste("Some wells are missing."))
  } else if (n > 96L) {
    warning("Duplicate wells---Results will be unpredictible./nCould this be a plate numbering issue?")
  }
  
  od_col <- paste0("A", wl)
  pic <- color_plate(rows=sub$AssayRow, cols=sub$AssayCol,
                     ODs=sub[, od_col, drop=TRUE],
                     minod=minod, maxod=maxod, palette)
  
  return(pic)
}