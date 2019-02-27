library(ggplot2)

source("global.R")

library(dplyr)


mfg_table <- get_mfg_table()
mfg_summary <- get_mfg_summary()

# For each plate, select just the low & high wells
low_wells <-
  plyr::ddply( mfg_table, "MfgPlate",
               function(dframe) {
                 minod <- min(dframe$A450)
                 lows <- which( dframe$A450 == minod )
                 dframe[lows, c("Well", "AssayRow", "AssayCol", "A450"),
                        drop=FALSE]
               } )

low_table <- 
  as.data.frame(table(low_wells$Well)) %>%
  rename(Well=Var1) %>%
  inner_join( get_wells() ) %>%
  arrange(Well) %>%
  mutate( RowOrder = ordered(AssayRow + (AssayCol-1)*8),
          ColOrder = ordered(AssayCol + (AssayRow-1)*12),
          AssayRow = as.ordered(AssayRow),
          AssayCol = as.ordered(AssayCol) ) %>%
  select(Well, AssayRow, AssayCol, RowOrder, ColOrder, Freq)



high_wells <-
  plyr::ddply( mfg_table, "MfgPlate",
               function(dframe) {
                 maxod <- max(dframe$A450)
                 highs <- which( dframe$A450 == maxod )
                 dframe[highs, c("Well", "AssayRow", "AssayCol", "A450"),
                        drop=FALSE]
               })

high_table <- 
  as.data.frame(table(high_wells$Well)) %>%
  rename(Well=Var1) %>%
  inner_join( get_wells() ) %>%
  arrange(Well) %>%
  mutate( RowOrder = ordered(AssayRow + (AssayCol-1)*8),
          ColOrder = ordered(AssayCol + (AssayRow-1)*12),
          AssayRow = as.ordered(AssayRow),
          AssayCol = as.ordered(AssayCol) ) %>%
  select(Well, AssayRow, AssayCol, RowOrder, ColOrder, Freq)



# Plot these:

plot_extremes <- function(tab, order="bycol") {
  # order = c( "byrow", "bycol" )
  if ( order == "byrow" ) {
    ord_s <- "RowOrder"
    fill_s <- "AssayCol"
  } else if ( order == "bycol" ) {
    ord_s <- "ColOrder"
    fill_s <- "AssayRow"
  }
  p <- ggplot() +
    geom_bar(
      aes_string( x=ord_s, y="Freq", fill=fill_s ), 
      data=tab, stat="identity") +
    theme( 
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank() )
  
  return(p)
}