library(stringr)


subset_wells <- function(tab, rowname="AssayRow", colname="AssayCol",
             expr="A1-H12") {

  if ( is.null(expr) || is.na(expr) ) {
    return(NULL)
  } else if ( gsub("\\s", "", expr) == "A1-H12" ) {
    # Just to save some time...
    tab_sub <- tab
  } else {
    selection <- select_wells(expr)
    tab_sub <- merge(tab, selection, by.x=c(rowname, colname),
                     by.y=c("Row", "Column"))    
  }
  
  return(tab_sub)
}


select_wells <- function(expr) {
  # Split the string into concatenated ranges:
  ranges <- 
    str_trim(str_split(expr, ",")[[1]])
  
  # Extract the corners of each range
  corners <-
    lapply(str_split(ranges, "-"),
           function(r) {
             # This should only have at most 2 elements:
             if (length(r) > 2) {
               warning("Improper range specification.")
               return(NULL)
             }
            return(str_trim(r))
          })
  
  selection <- plyr::ldply(corners, expand_range)
  selection <- unique(selection)
  
  return(selection)
}


expand_range <- function(rng) {
  # rng:  A character vector of length <= 2, giving well positions
  
    selection <- data.frame(Row=integer(0), Column=integer(0))
    
    if (length(rng)==0 | (length(rng)==1 & identical(rng, "")) ) { 
      warning("Empty range.")
      return(selection)
      
    }
    else if (length(rng) > 2) {
      warning("Improper range specification.")
      return(selection)
      
    } else {
      
      pat <- "^([A-H])(([1-9])|(1[0-2]))$"
      vals <- str_match(rng, pat)[, 2:3, drop=FALSE]
      
      if (any(is.na(vals)) == TRUE) {
        warning("Invalid wells specification.")
        return(selection)
      }

      # Translate row letters to numbers
      vals[,1] <- 
        chartr(paste0(LETTERS[1:8], collapse=''),
               paste0(1:8, collapse=''), vals[,1])
      # Convert to integers
      mode(vals) <- "numeric"
      
      # Now expand:
      if (nrow(vals) == 1) {
        # No need to expand
        rows <- vals[1,1]
        cols <- vals[1,2]
      } else if (nrow(vals) == 2) {
        rows <- seq(vals[1,1], vals[2,1])
        cols <- seq(vals[1,2], vals[2,2])
      }
      selection <- expand.grid(Row=rows, Column=cols)
    }  # length(rng)
    
    return(selection)
  }