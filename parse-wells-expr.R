source("select-wells.R")

library(stringr)
library(plyr)
library(dplyr)


parse_wells_expr <- function(wells) {
  
  groups <- parse_groups_list(wells)
  
  # Clean this up a bit
  
  # Collect together the ungrouped wells
  ungrouped <-
    groups %>%
    ldply(function(gr) {
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



# parse_well_range <- function(text) {
#   text <- str_trim(text)
#   mat <- str_match(text, "^([^-]*)\\s*-\\s*(.*)$")
#   
#   if (is.na(mat[[1]])) {
#     # No match; must be a single well
#     first_well <- parse_well(text)
#     last_well <- NULL
#   } else {
#     # Must be a well range
#     first_well <- parse_well(mat[[2]])
#     last_well <- parse_well(mat[[3]])
#   }
#   
#   well_rng <- 
#     list(Type="WellRange",
#          FirstWell=first_well, LastWell=last_well)
#   
#   return(well_rng)
# }


parse_well_range <- function(text) {
  rng_df <- select_wells(text)
  return(rng_df)
}


# parse_well <- function(text) {
#   text <- str_trim(text)
#   
#   mat <- str_match(text, "^([A-H])([0-9]|10|11|12)$")
#   
#   if (is.na(mat[[1]])) {
#     stop("Invalid well string.")
#   } else {
#     # Translate to a numeric value
#     row <- row_to_int(mat[[2]])
#     col <- as.integer(mat[[3]])
#     well <- list(Type="Well", Row=row, Column=col)
#     
#     return(well)
#   }
# }


parse_ungrouped_range_list <- function(text) {
  
  rng_df <- select_wells(text)
  
  return(rng_df)
}


# parse_ungrouped_range_list_2 <- function(text) {
#   text <- str_trim(text)
#   
#   mat <- str_match(text, "^([^,]*)(,(.*))?$")
#   well_rng <- str_trim(mat[[2]])
#   ungr_rng_tail <- str_trim(mat[[4]])
#   
#   if (is.na(mat[[1]])) {
#     # No match; Invalid expression
#     stop("Invalid ungrouped range list")
#   } else if (identical(ungr_rng_tail, "")) {
#     # This must be a well range
#     ungr_rng_lst <- list(parse_well_range(well_rng))
#   } else {
#     # This must be a list of well ranges
#     ungr_rng_lst <-
#       c(list(parse_well_range(well_rng)),
#         parse_ungrouped_range_list_2(ungr_rng_tail))
#   }
#   
#   return(ungr_rng_lst)
# }


## This should be useful...

row_to_int <- function(row) {
  stopifnot(identical(mode(row), "character"),
            identical(nchar(row), 1L),
            row %in% LETTERS[1:8])
  
  row_int <- 
    chartr(paste0(LETTERS[1:8], collapse=""),
         paste0(1:8, collapse=""), row)
  
  return(as.integer(row_int))
}



