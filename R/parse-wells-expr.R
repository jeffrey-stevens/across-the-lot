source("R/select-wells.R")

library(stringr)
library(dplyr)


parse_wells_expr <- function(wells) {
  
  groups <- parse_groups_list(wells)
  
  # Clean this up a bit
  
  # Collect together the ungrouped wells
  ungrouped <-
    groups %>%
    plyr::ldply(function(gr) {
      type <- gr$type
      df <- gr$df
      
      if (is.null(df) || identical(nrow(df), 0L)) {
        return(NULL)
        # Single grouped wells are equivalent to single ungrouped wells
      } else if (identical(type,"Ungrouped") ||
                 identical(nrow(df), 1L)) {
        return(df)
      } else {
        return(NULL)
      }
    })
  
  ungrouped <-
    if (!identical(nrow(ungrouped), 0L)) {
      ungrouped <-
        ungrouped %>%
        unique() %>%
        arrange(Row, Column)  # Is this desirable?
    } else {
      ungrouped <- NULL
    }

  
  # Now the grouped wells:    
  grouped <-
    lapply(groups, 
      function(gr) {
        type <- gr$type
        descr <- gr$descr
        df <- gr$df
        
        if (is.null(df) || identical(nrow(df), 0L)) {
          return(NULL)
        } else if (identical(type, "Grouped") &&
                    !identical(nrow(df), 1L)) {
          return(list(descr=descr, df=df))
        }
    })
  
  grouped <- Filter(function(gr) !is.null(gr), grouped)
    
  return(list(ungrouped=ungrouped, grouped=grouped))
}


parse_groups_list <- function(text) {
  text <- str_trim(text)
  pat <- "^((\\(([^\\(\\)]*)\\))|([^,\\(\\)]*))(,(.*))?$"
  mat <- str_match(text, pat)
  
  if (is.na(mat[[1]])) {
    warning("Invalid wells specification.")
    groups_list <- list(NULL)
  }
  
  paren <- str_trim(mat[[3]])
  grouped <- str_trim(mat[[4]])
  ungrouped <- str_trim(mat[[5]])
  rest <- str_trim(mat[[7]])
  
  if (!identical(ungrouped, "")) {
    # This is an ungrouped item
    group <- 
        list(type="Ungrouped", 
             descr=ungrouped,
             df=parse_well_range(ungrouped))
  } else if (!identical(grouped, "")) {
    group <- 
      list(type="Grouped", 
           descr=grouped,
           df=parse_well_range(grouped))
  } else {
    warning("Invalid wells specification.")
    groups_list <- list(NULL)
  }
  
  if (!identical(rest, "")) {
    groups_list <-
      c(list(group), parse_groups_list(rest))
  } else {
    groups_list <- list(group)
  }
    
  return(groups_list)
}


parse_well_range <- function(text) {
  rng_df <- select_wells(text)
  return(rng_df)
}


parse_ungrouped_range_list <- function(text) {
  
  rng_df <- select_wells(text)
  
  return(rng_df)
}


row_to_int <- function(row) {
  stopifnot(identical(mode(row), "character"),
            identical(nchar(row), 1L),
            row %in% LETTERS[1:8])
  
  row_int <- 
    chartr(paste0(LETTERS[1:8], collapse=""),
         paste0(1:8, collapse=""), row)
  
  return(as.integer(row_int))
}



