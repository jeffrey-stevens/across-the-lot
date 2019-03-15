source("R/global.R")

# dplyr conflicts with plyr
if ( "package:plyr" %in% search() ) {
  detach("package:plyr")
}
library(dplyr)

## Stacking map


# Reverse the ordering of the stacks:
reverse_stacks <- function(plates, origcol, newcol) {
  # newcol:  The name of the new column ("IncZ", for example)
  new_plates <- 
    plyr::ddply( plates, "Stack",
                 function(d) {
                   # Get top element:
                   n <- nrow(d)
                   top <- max( max( d[[origcol]], na.rm=TRUE ) )
                   
                   nc <- vector("integer", length=n)
                   # Start from the end of the source stack:
                   for ( i in seq_len(n) ) {
                     currz <- d[i, origcol]
                     
                     if ( is.na(currz) ) {
                       nc[[i]] <- NA
                     } else {
                       nc[[i]] <- top - currz + 1
                     }
                   }
                   
                   rev_stack <- data.frame(nc)
                   attr(rev_stack, "names") <- newcol
                   cbind(d, rev_stack)
                 })
  
  return(new_plates)
}


# Remove plates and recount the column
remove_plates <- function(plates, col, rem) {
  # origcol:  The column to remove plates from
  # newcol:  The name of the new ordering column
  # rem:  The mfg plates to remove
  
  new_plates <-
    plyr::ddply( plates, "Stack",
                  function(d) {
                    mfg_plates <- d$MfgPlate
                    n <- nrow(d)
                    
                    # Determine count direction
                    fst <- d[1, col]
                    lst <- d[n, col]
                    if ( fst <= lst ) {
                      set <- seq_len(n)
                    } else {
                      set <- rev(seq_len(n))
                    }
                    newz <- 1
                    
                    for ( i in set ) {
                      currplate <- mfg_plates[[i]]
                      currz <- d[i, col]
                      
                      if ( currplate %in% rem || is.na(currz) ) {
                         d[i, col] <- NA
                      } else {
                        d[i, col] <- newz
                        newz <- newz + 1
                      }
                    } # for
                    
                    return(d)
                  })
  
  return(new_plates)
}



build_stacks <- function(nrows=4L) {
  # This assumes that the plates stayed in the same stacks all the way
  # through...
  mfg_plates <- seq(MFG_MIN, MFG_MAX)
  stacks <- 
    data.frame( MfgPlate = mfg_plates,
                Stack = (mfg_plates-MFG_MIN) %/% 25 + 1,
                IdxInStack = (mfg_plates-MFG_MIN) %% 25 + 1)
  
  # X and Y locations of each stack, counting from the front-left
  # of the tray, working back then to the left:
  
  st <- unique(stacks$Stack)
  stack_pos <- 
    # st:  Stack sequence
    data.frame( Stack = st,
                StackRow = (st-1) %% nrows + 1,
                StackCol = (st-1) %/% nrows + 1)

  stacks_2 <-
    stacks %>%
    inner_join( stack_pos,
                by=c("Stack"="Stack") ) %>%
    arrange(MfgPlate)
  
  # The Pos in stack depends on the stage:
  
  discarded <- get_discarded_map()$MfgPlate
  
  stacks_3 <-
    stacks_2 %>%
    mutate( StagingZ = IdxInStack ) %>%  # Staging
    reverse_stacks( "StagingZ", "PostAgZ") %>% 
    remove_plates( "PostAgZ", discarded) %>%  # Ag coating
    reverse_stacks( "PostAgZ", "PostBlockZ" ) %>%
    reverse_stacks( "PostBlockZ", "PostSucroseZ" ) %>%
    reverse_stacks( "PostSucroseZ", "PostAspZ")
  
  return(stacks_3)
}


# Merge with the summary stats file
merge_data_stacks <- function(data_tab) {
  
  data_merged <- 
    data_tab %>%
    inner_join( build_stacks(), by = c("MfgPlate"="MfgPlate") )
  
  return(data_merged)
}


# Order plates according to their Z-order:
zlayer_order <- function(stacks, col, nrows=4L, ncols=6L) {
  
  x <- stacks[["StackCol"]]
  y <- stacks[["StackRow"]]
  z <- stacks[[col]]
  
  zorder <- (z - 1)*nrows*ncols + (x - 1)*nrows + y  

  cbind( stacks, ZOrder=zorder )  
}


z_well_order <- function(stacks, col, nrows=4L, ncols=6L) {
  sx <- stacks[["StackCol"]]
  sy <- stacks[["StackRow"]]
  sz <- stacks[[col]]
  wx <- stacks[["AssayCol"]]
  wy <- stacks[["AssayRow"]]
  
  x <- (sx - 1)*12 + wx
  y <- (sy - 1 )*8 + wy
  
  cbind(stacks, WellX=x, WellY=y)   
}


source("R/plots/layers.R")

library(ggplot2)



plot_heatmap <- function(k=1) {
  ggplot( data = subset(data_merged_summary, StagingZ == k) ) +
    geom_tile( aes( x=StackCol, y=StackRow, fill=Mean450) ) +
    scale_fill_continuous( limits=c(0.25, 1) )
}


z_wells <- z_well_order( merge_data_stacks(get_mfg_table() ),
                         "StagingZ")
plot_heatmap_2 <- function(k=1) {
  ggplot( data = subset(z_wells, StagingZ == k) ) +
    geom_tile( aes( x=WellX, y=WellY, fill=A450) ) +
    scale_fill_continuous( low="black", high="white",
                           limits=c(0.25, 1) )
}

library(manipulate)
manipulate(plot_heatmap_2(k), k=slider(1,25))


