source("R/plots/plots/mfg-plot-2.R")

library(shiny)
library(ggvis)


build_server <- function(readings, runsmap, msa_table, mfg_table,
                         mfg_summary) {
  
  # Build the summary table:
  
  
  function(input, output, session) {
    
    options(shiny.usecairo=FALSE)
      
    output$runplot <- 
      renderPlot({
        p <- plot_shift(day=input$day, shift=input$shift,
                   wl=input$wl, readings=readings, runsmap=runsmap,
                   yrange=input$run.yrange)
        show(p)
      }, type="windows")
    
    
    output$msaplot <-
      renderPlot({
        p <- plot_msa(msa_table)
        show(p)
      }, type="windows")

    
    output$visplate <-
      renderPlot({
        # Need to translate the plate to matrix form!
        p <- 
          show_mfg_plate(mfg_table, plate=input$mfgplate,
                         minod=input$odscale[[1]], maxod=input$odscale[[2]],
                         palette=input$palette)
        grid.draw(p)
      })
  
    
    output$mfgvar <-
      renderPlot({
  
        plot_options <- function(names) {
          vals <- names %in% input$options
          names(vals) <- names
          return(as.list(vals))
        }
        
        opts <- plot_options(c("color.runs", "jitter"))
        
        p <- 
          do.call(plot_mfg,
                  c(list(mfg_table=mfg_table, mfg.range=input$mfgrange,
                         od.range=input$odrange,
                         wells=input$wells,
                         od.geoms=input$geoms, var.plot=input$varplot,
                         alpha=0.4),
                    opts))
        show(p)
      }, type="windows")
    
    
    output$mfg.mfgorder.ui <-
      renderUI({
        
        if ( is.na(input$mfg.xrange) ||
             is.na(input$mfg.nplates) ) return()
        
        xmin <- input$mfg.xrange[[1]]
        xmax <- input$mfg.xrange[[2]]
        
        # Don't do anything if the input is too small:
        nplates <- input$mfg.nplates
        go <- !is.na(nplates) && (nplates >= max(10, xmin))
        
        if ( go ) {
          # Estimate the height of each plot:
          tot_plates <- xmax - xmin + 1
          nplots <- ceiling(tot_plates / nplates)
          h <- 200 * nplots  # 200 px per plot
          plotOutput("mfg.mfgorder.plot", height=paste0(h, "px"))
        }
      })
    
    
    # JRS 1/10/2019
    # May need an observe({ ... updateSliderInput(...) }) here...
    
    
    output$mfg.mfgorder.plot <-
      renderPlot({

        ## Get layers
        layers <- c( input$mfg.mfgorder.type,
                     input$mfg.mfgorder.layers)
        # Not all layers are included here:
        bandsize <- input$mfg.bandsize
        if ( !is.na(bandsize) && bandsize > 0 ) {
          # Add a 'hilight' layer:
          layers <- c(layers, "hilight")
        }
        
        if ( is.null(layers) || is.na(layers) ) return()
        
        opts <- get_opts(input, layers)
        
        # Debug
        cat(paste0("Rendering plot...xrange = ",
                   input$mfg.xrange[[1]], ", ", input$mfg.xrange[[2]],
                   "\n"))
        
        p <- 
          plot_mfg_2(mfg_table, mfg_summary,
                     wells=input$mfg.mfgorder.wells, wl=450,
                     layers=layers, nplates=input$mfg.nplates,
                     xrange=input$mfg.xrange, ylim=input$mfg.yrange, 
                     opts=opts)
        
        drawing <- grid.draw(p)  
        
      }, type="windows")
    
  
    output$gradplot <-
      renderPlot({
        p <- plot_gradient(mfg_table, type=input$gradplottype,
                           margin=input$gradoptions)
        show(p)
      }, type="windows")
    
    
    ## Interactive plot
    
    user <- reactiveValues( sel_df = NULL)
    
    output$mfg.active.data <- renderDataTable({ user$sel_df })
    
    
    do_brush <- function(items, page_loc, plot_loc, session) {
      if ( is.null(items) || nrow(items) == 0 ) return ()
      
      sel_df <- filter_by_id( mfg_table, items$WellID )
      show(sel_df)
      user$sel_df <- sel_df
    }
    
    p <- 
      reactive({
        xrange <- input$mfg.active.xrange
        yrange <- input$mfg.active.yrange
        coloring <- input$mfg.active.coloring
        
        if ( is.null(xrange) || is.na(xrange) ||
             is.null(yrange) || is.na(yrange) ||
             is.null(coloring) || is.na(coloring) ) {
          NULL
          
        } else {
          plot_mfg_ggvis(mfg_table, xrange=xrange, yrange=yrange,
                         brushHandler=do_brush,
                         coloring=coloring)
        }
        
      }) %>%
      bind_shiny("mfgActivePlot", session=session)
    
    #if ( !is.null(p) ) {
   
    #}

  }
  
}



get_opts <- function(input, layers) {
  xmin <- input$mfg.xrange[[1]]
  xmax <- input$mfg.xrange[[2]]
  coloring <- input$mfg.mfgorder.coloring
  if ( is.null( coloring ) || is.na( coloring ) ) {
    coloring <- "none"
  }
  
  opts <- list()
  for (lyr in layers) {

    # Points
    if ( lyr == "points" ) {
      opts[["points"]] <- list( coloring=coloring )
    }
    
    # Band hilighting
    if ( lyr == "hilight" ) {
      bandsize <- input$mfg.bandsize
      if ( is.na(bandsize) || bandsize < 0 ) return()
      opts[["hilight"]] <- list(offset=xmin, interval=bandsize)
    }
    
    # Parallel coordinates
    if ( lyr == "parcoord" ) {
      opts[["parcoord"]] <- list( alpha = 0.2 )
    }
  }
  
  return(opts)
}
