# Compare the top row to the bottom row

source( "global.R" )

library( dplyr )
library( tidyr )


if ( !exists( "mfg_table" ) ) {
  mfg_table <- get_mfg_table()  
}


top_bottom <-
  mfg_table %>%
  group_by( MfgPlate, AssayRow) %>%
  filter( AssayRow == 1 | AssayRow == 8) %>%
  summarize( RowMean = mean(A450) ) %>%
  mutate( AssayRow = paste0("Row", AssayRow) ) %>%
  spread( key = AssayRow, value = RowMean  ) %>%
  mutate( Ratio = Row1 / Row8 )

with( top_bottom, table( Category = Ratio > 1 ) )

# Actually, there's a bottom-top effect here.  Let's plot it:

library( ggplot2 )

p <- 
  ggplot( data = top_bottom, aes( Ratio ) ) +
  geom_histogram( fill = "blue", color = "black" ) +
  theme_bw()