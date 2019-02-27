source("layers.R")
source("select-wells.R")


plot_mfg_2 <- function(mfg_table, mfg_summary=NULL,
                       wells="A1-H12", wl=450L,
                       layers="points", nplots=NULL, nplates=NULL,
                       xrange=NULL, ylim=NULL, opts=NULL) {
  # nplates:  The number of mfg plates per plot
  # ylim:     The limits of the clipping window
  # opts:     list([layer]=[opts])

  # Exactly one of nplots and nplates must be NULL:
  if ( is.null(nplots) && is.null(nplates) ) {
    warning("Neither 'nplots' nor 'nplates' is given; assuming 1 plot.")
    nplots <- 1L
  } else if ( !is.null(nplots) && ! is.null(nplates) ) {
    stop("Only one of 'nplots' or 'nplates' must be given.")
  }
  
  # Convert the wavelength to text:
  wltxt <- paste0("A", wl)  # Assumes that a valid wavelength is given...
  
  # Get the full (unclipped) ranges:
  if (is.null(xrange)) xrange <- range(mfg_table$MfgPlate)
  yrange <- c(0, max(mfg_table[,wltxt,drop=TRUE]))
  # Note that ylim=NULL is a valid input to coord_cartesian...
  
  # Now subset the wells, if necessary
  tab_sub <- subset_wells(mfg_table, rowname="AssayRow",
                          colname="AssayCol", expr=wells)
  # Note that this could be null...
  
  if (is.null(tab_sub)) {
    p <- NULL
  } else {
    # Construct the primary plot:
    prmy_plot <- 
      build_layers(tab_sub, mfg_summary, wl, layers, xrange, yrange, opts)  
    # Now break this up:
    plot_list <- 
      breakup_plot(prmy_plot, xrange, yrange, ylim, nplots, nplates)
    # Now splice all of them together:
    p <- do.call(multiplot, plot_list)
  }
  
  return(p)
}


build_layers <- function(mfg_table, mfg_summary, wl, layers,
                         xrange, yrange, opts) {
  
  if (is.null(layers)) {
    lyr_list <- list(geom_blank()) 
  } else {
    lyr_list <-
      lapply(layers,
             function(lyr) {
               build_layer(mfg_table, mfg_summary, wl=wl, lyr,
                           xrange, yrange, opts[[lyr]])
             }) 
  }
  
  p <- 
    build_od_frame(xrange, yrange) +
    lyr_list
  
  return(p)
}


build_layer <- function(mfg_table, mfg_summary,
                        wl, lyr, xrange, yrange, opts) {
  if ( lyr == "none" ) {
    geom_blank()
  } else if (lyr == "points") {
    points_layer(mfg_table, wl, jitter=TRUE, coloring=opts$coloring)
  } else if ( lyr == "hilight" ) {
    hilight_layer(xrange=xrange, yrange=yrange,
                  offset=opts$offset, interval=opts$interval)
  } else if ( lyr == "means" ) {
    means_layer(mfg_summary, wl=wl)
  } else if ( lyr == "medians" ) {
    medians_layer(mfg_summary, wl=wl)
  } else if ( lyr == "connmean" ) {
    connected_means_layer(mfg_summary, wl=wl)
  } else if ( lyr == "connmed" ) {
    connected_medians_layer(mfg_summary, wl=wl)
  } else if ( lyr == "parcoords" ) {
    par_coord_layer(mfg_table, alpha=opts$alpha)
  } else {
    warning( "Unknown layer type." )
    geom_blank()
  }
}


breakup_plot <- function(p, xrange, yrange, ylim=NULL,
                         nplots=NULL, nplates=NULL) {
  # Exactly one of nplots and nplates must be NULL:
  if (is.null(nplots) && is.null(nplates)) {
    warning("Neither 'nplots' nor 'nplates' is given; assuming 1 plot.")
    nplots <- 1L
  } else if (!is.null(nplots) && ! is.null(nplates)) {
    stop("Only one of 'nplots' or 'nplates' must be given.")
  }
  
  if (!is.null(nplots)) {
    # Break up according to the number of plots:
    breaks_df <- get_n_plots(xrange, nplots)
  } else {
    # Break up according to the number of plates per plot
    breaks_df <- get_nplates_plots(xrange, nplates)
  }

  # Now break this up:
  if (is.null(ylim)) ylim <- yrange
  ylims <- c(max(0, ylim[[1]] - 0.02), ylim[[2]] + 0.02)
  plot_list <-
    lapply(seq_len(nrow(breaks_df)),
           function(n) {
             xlims <- as.numeric(breaks_df[n,])
             p + coord_cartesian(xlim=xlims, ylim=ylims)
           })
  
  return(plot_list)
}


get_n_plots <- function(xrange, nplots=1L) {
  # Get the number of plots
  if (nplots < 1L) nplots <- 1L
  
  # Get the division size
  nplates <- diff(xrange) + 1
  divsize <- ceiling(nplates/nplots)
  
  # If the div size is too small, then reduce the number of
  # plots until the div size is large enough:
  warn <- FALSE
  while (divsize < 25 && nplots > 1) {
    nplots <- nplots - 1
    divsize <- ceiling(nplates/nplots)
    warn <- TRUE
  }
  # If nplots = 1 now, then it's okay that the divsize is less than 25...
  if (warn) {
    warning(paste0("Reducing the number of plots to ", nplots, "."))    
  }
  
  # Calculate divisions, padding the end if necessary:
  brkmin <- xrange[[1]] + (seq_len(nplots) - 1) * divsize
  brkmax <- xrange[[1]] + seq_len(nplots) * divsize - 1
  
  # Now expand these a bit to prevent clipping of jittered points:
  brkmin <- brkmin - 0.5
  brkmax <- brkmax + 0.5
  
  df <- data.frame(Min=brkmin, Max=brkmax)
  return(df)
}


get_nplates_plots <- function(xrange, nplates = diff(xrange) + 1) {
  xmin <- xrange[[1]]
  xmax <- xrange[[2]]
  
  # Calculate all the beginning-of-range plates:
  begnmin <- 0
  begnmax <- floor(diff(xrange)/nplates)
  begn <- seq(begnmin, begnmax)
  beg <- xmin + begn * nplates

  # Keep the scales the same...
  end <- beg + nplates - 1

  # Now expand these a bit to prevent clipping of jittered points:
  beg <- beg - 0.5
  end <- end + 0.5
  
  df <- data.frame(Min=beg, Max=end)
  return(df)
}