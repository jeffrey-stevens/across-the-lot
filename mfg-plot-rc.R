source("layers.R")

library(ggplot2)
library(R6)
library(dplyr)

MFG_DATA_FILE <- "../Data tables/MfgMastertable.csv"


ODPlot <- 
  R6Class("ODPlot",
    public=list(
      data_tab=NULL,
      summary_tab=NULL,
      run_order=NULL,
      range_mfg=NULL,
      range_A450=NULL,
      range_A650=NULL,
      subrange_mfg=NULL,
      subrange_A450=NULL,
      subrange_A650=NULL,
      wavelength=450,
      
      initialize=function(data_tab=NULL) {
        
        if (is.null(data_tab)) {
          data_tab <- read.csv(MFG_DATA_FILE)
        }
        
        ## Set the ranges
        self$range_mfg=range(data_tab$MfgPlate)
        self$range_A450=c(0, max(data_tab$A450))
        self$range_A650=c(0, max(data_tab$A650))
        self$subrange_mfg <- self$range_mfg
        self$subrange_A450 <- self$range_A450
        self$subrange_A650 <- self$range_A650
        
        ## Add additional columns
        
        self$run_order <-
          data_tab %>%
          dplyr::select(Day, Shift) %>%
          unique() %>%
          dplyr::arrange(Day, Shift) %>%
          dplyr::mutate(RunOrder=seq_len(n()))
        
        private$well_names <- 
          expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
          mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                                    paste0(LETTERS[1:8], collapse=""),
                                    AssayRow),
                             AssayCol),
                 WellOrder=as.numeric(seq_len(n())))
        # Well ordering is right!
        # May want to store this...
        
        # Now merge with the data table:
        data_tab <- 
          data_tab %>%
          inner_join(private$well_names) %>%
          inner_join(self$run_order)
        # This may upset the order of the entries...
        
        # Now store:
        self$data_tab <- data_tab
        
        
        ## Calculate summary stats (for all data)
        self$summary_tab <-
          plyr::ddply(self$data_tab, 
                      c("Day", "Shift", "Run", "MfgPlate", "RunOrder"),
                      function(df) {
                        # How would this behave if no input were given?
                        m <- mean(df$A450)
                        s <- sd(df$A450)
                        cv <- ifelse(is.na(s) | m==0, 0, s/m*100)
                        r <- diff(range(df$A450))
                        rr <- r/m*100
                        c(MeanOD=m, StDev=s, CV=cv, Range=r, RelRange=rr)
                      })
      },
      
      add_A450_layer=function(layer, ...) {
        p <-
          switch(layer,
               "points"=points_layer(self$data_tab, wl=450, ...),
               NULL)
        
        private$A450_layers <- append(private$od_layers, p)
        
        return(self)
      },
      
      clear_layers=function() {
        # To do...
      },
      
      build_od_plot=function(wl=self$wavelength) {
        xrange <- self$range_mfg
        xsubrange <- self$subrange_mfg
        
        if (identical(wl, 450)) {
          yrange <- self$range_A450
          ysubrange <- self$subrange_A450
          lyrs <- private$A450_layers
        } else if (identical(wl, 650)) {
          yrange <- self$range_A650
          ysubrange <- self$subrange_A650
          lyrs <- private$A650_layers
        }
        
        build_od_frame(xrange, yrange) + 
          coord_cartesian(xsubrange, ysubrange) +
          lyrs
      }
    ),
    
    private=list(
      well_names=NULL,
      A450_layers=list(geom_blank()),
      A650_layers=list(geom_blank()),
      var450_layers=list(geom_blank()),
      var650_layers=list(geom_blank()),
      subset=NULL,
      subset_expr=NULL,
      subset_tab=NULL,
      make_subset=function(expr) {
        # expr:  Quoted expression
        private$subset_expr <- expr
        
        private$subset <- 
          which(self$data_tab,
                which(eval(expr)))
        
        private$subset_tab <- self$data_tab[private$subset,,drop=FALSE]
      }
      )
  )







