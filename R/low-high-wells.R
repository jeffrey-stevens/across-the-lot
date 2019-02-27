source("R/global.R")

library(ggplot2)
library(plyr)


mfg_table <- get_mfg_table()
mfg_summary <- get_mfg_summary()

# For each plate, select just the low & high wells
low_wells <-
  ddply( mfg_table, .(MfgPlate),
         function(dframe) {
           minod <- min(dframe$A450)
           lows <- which( dframe$A450 == minod )
           dframe[lows, c("Well", "AssayRow", "AssayCol", "A450"),
                  drop=FALSE]
         } )

low_table <- as.data.frame(table(low_wells$Well))

low_counts <-
  ddply( low_wells, .(Well),
         function(dframe) {
           cbind(dframe[1, c("Well", "AssayRow", "AssayCol")],
                 Count=nrow(dframe))
         } )

high_wells <-
  ddply( mfg_table, .(MfgPlate),
         function(dframe) {
           maxod <- max(dframe$A450)
           highs <- which( dframe$A450 == maxod )
           dframe[highs, c("Well", "AssayRow", "AssayCol", "A450"),
                  drop=FALSE]
         })

high_table <- as.data.frame(table(high_wells$Well))

high_counts <-
  ddply( high_wells, .(Well),
         function(dframe) {
           cbind(dframe[1, c("Well", "AssayRow", "AssayCol")],
                 Count=nrow(dframe))
         } )


#dmultinom(table(low_counts$Well), prob=rep(1/96, 96))
#dmultinom(table(high_counts$Well), prob=rep(1/96, 96))

