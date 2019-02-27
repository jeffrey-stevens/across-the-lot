source("select-wells.R")

library(ggplot2)
library(gridExtra)


###  Mfg order layers  ------------------------

points_layer <- function(data_tab, wl=450, jitter=FALSE,
                         coloring="byrun") {
  # coloring:  c("none", "byrun", "bywell")
  
  # Select the wavelength
  if (wl == 450) {
    wl_q <- quote(A450)
  } else if (wl == 650) {
    wl_q <- quote(A650)
  } else {
    stop("'wl' is not a valid wavelength.")
  }
  
  # Add jitter?
  jit <- ifelse(jitter, 0.3, 0)
  
  # Coloring
  if ( is.null(coloring) || is.na(coloring) ) {
    coloring <- "none"
  }
  col_q <- switch(coloring,
                none = NULL,
                byrun = quote(RunOrder),
                bywell = quote(WellOrder),
                default = NULL)
  
  # Unfortunately, ggplot doesn't allow you to compare continuous
  # and discrete color scales, even if they're in different layers...
  # Consequently, the "points" and "par coords" plots can't be mixed...
  points_aes <- aes_q(x=quote(MfgPlate), y=wl_q, color=col_q)
  
  geom_point( points_aes, data=data_tab,
             size=1, position=position_jitter(jit) )
}



hilight_layer <- function(xrange, yrange,
                          offset=1, interval=25,
                          color1="blue", color2="red",
                          alpha=0.1) {
  # offset:  1 means start from 1 and end at 'interval'
  
  stopifnot(xrange[[1]] <= xrange[[2]] && yrange[[1]] <= yrange[[2]])
  
  # Get the divisions
  divisions <- get_divisions(xrange, offset, interval)
  n <- nrow(divisions)
  
  # Alternate the colors of the regions
  fill <- as.factor(as.integer((seq_len(n) - 1) %% 2 + 1))
  
  df <- data.frame(division=seq_len(n),
                   xmin=divisions$Begin, xmax=divisions$End, 
                   ymin=yrange[[1]], ymax=yrange[[2]],
                   fill=fill)
  
  list(
    geom_rect(aes(xmin=xmin, xmax=xmax,
                  ymin=ymin, ymax=ymax,
                  fill=fill), data=df, alpha=alpha),
    scale_fill_manual(values = c("1" = color1, "2" = color2))
    )
  
}


get_divisions <- function(xrange, offset=1, interval=25) {
  xmin <- xrange[[1]]
  xmax <- xrange[[2]]
  
  # Find the divisions
  nmin <- ceiling((xmin - offset)/interval)
  nmax <- floor((xmax - offset + 1)/interval)
  
  # Test the boundaries
  if (nmax < nmin) {  # The empty set case!
    begs <- xmin
    ends <- xmax
  } else {
    ns <- seq(nmin, nmax)
    
    # Round up from the -0.5 boundary...
    begs <- offset + ns * interval
    nbegs <- length(begs)
    # Expand the set if to include xmin
    if (xmin < begs[1]) { begs <- c(xmin, begs) }
    # Test the case that rounding up sends us out-of-range
    if (begs[nbegs] > xmax) { begs <- begs[-nbegs] }
    
    # Round down from the -0.5 boundary...
    ends <- offset + ns * interval - 1
    nends <- length(ends)
    # Expand the set to include xmax
    if (xmax > ends[nends]) { ends <- c(ends, xmax) }
    # Test the case that rounding down sends us out-of-range
    if (ends[1] < xmin) { ends <- ends[-1] }    
  }
  
  # Now expand by 0.5:
  begs <- begs - 0.5
  ends <- ends + 0.5
  
  return(data.frame(Begin=begs, End=ends))
}


par_coord_layer <- function(data_tab, alpha=NULL) {

  if ( is.null(alpha) || is.na(alpha) ) {
    alpha <- 0.3
  }
  
  geom_line(aes(x=MfgPlate, y=A450, color=Well),
            data=data_tab, alpha=alpha)
}


missing_points_layer <- function(tab_clip_low, tab_clip_high) {
  # Add rugs for the clipped points
  rug_bottom <- 
    geom_rug(aes(x=MfgPlate, y=A450), data=tab_clip_low,
             position="jitter",
             color="red3", alpha=0.5, sides="b")
  rug_top <- 
    geom_rug(aes(x=MfgPlate, y=A450), data=tab_clip_high,
             position="jitter",
             color="red3", alpha=0.5, sides="t")
  
  return(list(bottom=rug_bottom, top=rug_top))
}



means_layer <- function(summary_tab, wl=450) {
  wltxt <- paste0("Mean", wl)
  
  df <- data.frame(x=summary_tab$MfgPlate - 0.5,
                   xend=summary_tab$MfgPlate + 0.5,
                   y=summary_tab[[wltxt]])
  
  means_plot <- 
    geom_segment(aes(x=x, y=y, xend=xend, yend=y),
                 data=df, color="red", size=1)
  
  return(means_plot)
}


medians_layer <- function(summary_tab, wl=450) {
  wltxt <- paste0("Median", wl)
  
  df <- data.frame(x=summary_tab$MfgPlate - 0.5,
                   xend=summary_tab$MfgPlate + 0.5,
                   y=summary_tab[[wltxt]])
  
  medians_plot <- 
    geom_segment(aes(x=x, y=y, xend=xend, yend=y),
                 data=df, color="red", size=1)
  
  return(medians_plot)
}


connected_means_layer <- function(summary_tab, wl) {

  wltxt <- paste0("Mean", wl)
  
  df <- data.frame(x=summary_tab$MfgPlate,
                   y=summary_tab[[wltxt]])
  
  connected_plot <- geom_line(aes(x=x, y=y),
                              data=df, color="red2")
  
  return(connected_plot)
}


connected_medians_layer <- function(summary_tab, wl) {
  
  wltxt <- paste0("Median", wl)
  
  df <- data.frame(x=summary_tab$MfgPlate,
                   y=summary_tab[[wltxt]])
  
  connected_plot <- geom_line(aes(x=x, y=y),
                              data=df, color="red2")
  
  return(connected_plot)
}


### Variability layers -----------------


var_layer  <- function(summary_tab, varstat="cv", colorRuns=FALSE) {
  
  # Variability plot
  if (varstat %in% c("sd", "cv", "range", "relrange") & !emptytab) {
    
    if (colorRuns) f <- quote(RunOrder) else f <- NULL
    h <- switch(varstat,
                "sd"=quote(StDev),
                "cv"=quote(CV),
                "range"=quote(Range),
                "relrange"=quote(RelRange))
    ymax <- switch(varstat,
                   "sd"=max(summary_tab$StDev),
                   "cv"=max(summary_tab$CV),
                   "range"=max(summary_tab$Range),
                   "relrange"=max(summary_tab$RelRange))
    ylab <- switch(varstat,
                   "sd"="StDev (OD)\n",
                   "cv"="% CV\n",
                   "range"="Range (OD)\n",
                   "relrange"="% Range\n")
    var_aes <- aes_q(xmin=quote(MfgPlate-0.5), xmax=quote(MfgPlate+0.5),
                     ymin=0, ymax=h, fill=f)
    var_bars <- 
      geom_rect(var_aes, data=summary_tab, width=0)
    
    var_plot <- list(var_bars, ylim(0, ymax), ylab(ylab))
    
  } else if (identical(varstat, "none")) {
    var_plot <- NULL
    
  } else {
    warning("'varstat' must be one of 'sd', 'cv', 'range' or 'relrange'.")
    var_plot <- NULL
  }
}



### Plot building functions  ----------------------------

build_od_frame <- function(xrange, yrange) {
  # Expand the xrange by 0.5 to prevent clipping
  xrange[[1]] <- xrange[[1]] - 0.5
  xrange[[2]] <- xrange[[2]] + 0.5
  od_frame <-
    ggplot() +
    theme_bw() + theme(legend.position="none") +
    xlim(xrange) + ylim(yrange + c(-0.02, 0.02)) +
    labs(x=NULL, y="A450\n") +
    geom_blank()
  
  return(od_frame)
}


build_var_frame <- function() {
  var_frame <-
    ggplot() +
    theme_bw() + theme(legend.position="none") +
    xlim(xrange) + xlab("\nMfg plate") +
    geom_blank()
  
  return(var_frame)
}


multiplot <- function(...) {
  # ...:  ggplots
  plots <- list(...)
  # Get the axes to line up by making the y-axis labels the same width
  # between the plots:
  gtabs <- 
    lapply(plots, 
           function(p) {
              ggplot_gtable(ggplot_build(p))
           })
  
  # Reset the widths of each
  widths <- lapply(gtabs, function(gt) gt[[3]] )
  y_axis_width <- do.call(max, widths)
  lapply(gtabs, function(gt) gt$widths[[3]] <- y_axis_width)
  
  newplot <- 
    do.call(arrangeGrob,
      c(gtabs, list(nrow=length(plots), ncol=1))
    )
  
  return(newplot)
}