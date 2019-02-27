# Todo:
#
#  * Merge the CV plot with the points plot & flip upside-down? (Cool!)
#  * Add a title to the plot?
#  ** Warn if points are clipped..
#  * Plate grouping patterns (every 100th plate?)
#  * x-axis/grid:  Every 100th plate?  Adjust to the range...
#  * Adjust alpha with # of points plotted in parallel...
#  * Fix the calculation of the grand mean
#  * Fix clipping of the ends...
#  * Par coord:  If specify subsection of the data set, does it reset
#    the group coloring???



source("parse-wells-expr.R")

library(ggplot2)
library(gridExtra)
library(dplyr)
library(gtable)


plot_mfg <- function(mfg_table, mfg.range=range(mfg_table$MfgPlate),
                     od.range=NULL,
                     wells="A1-H12",
                     od.geoms="points", var.plot="cv", alpha=1,
                     color.runs=FALSE, jitter=FALSE, 
                     color.wells=FALSE) {
  # od.geoms=c("points", "means", "connected", "parcoord")
  # var.plot:  c("sd", "cv", "range", "relrange" "none")
  
  
  # First, get the wells to include:
  selection <- select_wells(wells)
  

  table_sub <- 
    mfg_table %>%
    filter(between(MfgPlate, mfg.range[[1]], mfg.range[[2]])) %>%
    inner_join(selection, by=c(AssayRow="Row", AssayCol="Column"))
  
  
  emptytab <- identical(nrow(table_sub), 0L)

  summary_tab <-
    table_sub %>%
    group_by(Day, Shift, Run, MfgPlate, RunOrder) %>%
    summarise(MeanOD=mean(A450), StDev=sd(A450),
              Range=diff(range(A450)))  %>%
    mutate(CV=StDev/MeanOD*100, RelRange=Range/MeanOD*100)
  
#     plyr::ddply(table_sub, c("Day", "Shift", "Run", "MfgPlate", "RunOrder"),
#               function(df) {
#                 # How would this behave if no input were given?
#                 m <- mean(df$A450)
#                 s <- sd(df$A450)
#                 cv <- ifelse(is.na(s) | m==0, 0, s/m*100)
#                 r <- diff(range(df$A450))
#                 rr <- r/m*100
#                 c(MeanOD=m, StDev=s, CV=cv, Range=r, RelRange=rr)
#               })
  
  # If an OD range isn't specified, then adjust it to the range
  # of the current plot
  if (is.null(od.range)) od.range <- c(0, max(mfg_table$A450)) 
  
  # Clip out-of-range points:
  tab_clip_low <- 
    table_sub %>% filter(A450 < od.range[[1]])
  tab_clip_high <-
    table_sub %>% filter(A450 > od.range[[2]])
  tab_clip_bet <- 
    table_sub %>% filter(between(A450, od.range[[1]], od.range[[2]]))
  
  # Clip out-of-range points (may not be necessary here...)
  sum_clip_low <- 
    summary_tab %>% filter(MeanOD < od.range[[1]])
  sum_clip_high <-
    summary_tab %>% filter(MeanOD > od.range[[2]])
  sum_clip_bet <- 
    summary_tab %>% filter(between(MeanOD, od.range[[1]], od.range[[2]]))
  
  
  ## Create the plot frames
  
  od_frame <-
    ggplot() +
    theme_bw() + theme(legend.position="none") +
    xlim(mfg.range) + ylim(od.range + c(-0.02, 0.02)) +
    labs(x=NULL, y="A450\n") +
    geom_blank()
  
  var_frame <-
    ggplot() +
    theme_bw() + theme(legend.position="none") +
    xlim(mfg.range) + xlab("\nMfg plate") +
    geom_blank()
  
  
  ## Geoms
  
  if ("points" %in% od.geoms & !emptytab) {
    
    # Add jitter?
    jit <- ifelse(jitter, 0.3, 0)
    
    # Color by when the plate was run?
    if (color.runs) clr <- quote(RunOrder) else clr <- NULL
    
    points_aes <- aes_q(x=quote(MfgPlate), y=quote(A450), color=clr)
    points_plot <- 
        geom_point(points_aes, data=tab_clip_bet,
          size=1, position=position_jitter(jit))
    
  } else {
    points_plot <- geom_blank()
  } 
  
  
  ## These may have to be edited for clipping...
  
  # Plot means
  if ("means" %in% od.geoms & !emptytab) {
    means_plot <- 
         geom_segment(aes(x=MfgPlate-0.5, y=MeanOD,
                       xend=MfgPlate+0.5, yend=MeanOD),
                   data=summary_tab, color="red", size=1)
  } else {
    means_plot <- geom_blank()
  }
  
  
  # Connect mean values
  if ("connect" %in% od.geoms & !emptytab) {
    connected_plot <- geom_line(aes(x=MfgPlate, y=MeanOD), data=summary_tab,
               color="red2")
  } else {
    connected_plot <- geom_blank()
  }
  
  
  # Parallel coordinates
  if ("parcoord" %in% od.geoms & !emptytab) {
    parcoord_plot <- 
      geom_line(aes(x=MfgPlate, y=A450, color=Well),
              data=tab_clip_bet, alpha=alpha)
  } else {
    parcoord_plot <- geom_blank()
  }
  
  
  # Add rugs for the clipped points
  rug_bottom <- 
    geom_rug(aes(x=MfgPlate, y=A450), data=tab_clip_low,
             position="jitter",
             color="red3", alpha=0.5, sides="b")
  rug_top <- 
    geom_rug(aes(x=MfgPlate, y=A450), data=tab_clip_high,
             position="jitter",
             color="red3", alpha=0.5, sides="t")
  
  
  # Construct the OD plot
  od_plot <-
    od_frame + points_plot + means_plot + connected_plot +
    parcoord_plot + rug_bottom + rug_top
  
  
  # Variability plot
  if (var.plot %in% c("sd", "cv", "range", "relrange") & !emptytab) {
    if (color.runs) f <- quote(RunOrder) else f <- NULL
    h <- switch(var.plot,
                   "sd"=quote(StDev),
                   "cv"=quote(CV),
                   "range"=quote(Range),
                  "relrange"=quote(RelRange))
    ymax <- switch(var.plot,
                   "sd"=max(summary_tab$StDev),
                   "cv"=max(summary_tab$CV),
                   "range"=max(summary_tab$Range),
                   "relrange"=max(summary_tab$RelRange))
    ylab <- switch(var.plot,
                   "sd"="StDev (OD)\n",
                   "cv"="% CV\n",
                   "range"="Range (OD)\n",
                   "relrange"="% Range\n")
    var_aes <- aes_q(xmin=quote(MfgPlate-0.5), xmax=quote(MfgPlate+0.5),
                    ymin=0, ymax=h, fill=f)
    var_bars <- 
      geom_rect(var_aes, data=summary_tab#, 
                #width=0)
      )
    var_plot <- var_frame + var_bars + ylim(0, ymax) + ylab(ylab)
  } else if (identical(var.plot, "none")) {
    var_plot <- NULL
  } else {
    warning("'var.plot' must be one of 'sd', 'cv', 'range' or 'relrange'.")
    var_plot <- NULL
  }
  
  ## Now fit the two together, if necessary
  if (is.null(var_plot)) {
    finalplot <- od_plot
  } else {
    finalplot <- multiplot(od_plot, var_plot)
  }
  
  return(finalplot)
}


multiplot <- function(plot1, plot2) {
  # Get the axes to line up by making the y-axis labels the same width
  # between the plots:
  plot1_gtab <- ggplot_gtable(ggplot_build(plot1))
  plot2_gtab <- ggplot_gtable(ggplot_build(plot2))
  y_axis_width <- max(plot1_gtab$widths[[3]], plot2_gtab$widths[[3]])
  plot1_gtab$widths[[3]] <- y_axis_width
  plot2_gtab$widths[[3]] <- y_axis_width
  
  newplot <- arrangeGrob(
    plot1_gtab, plot2_gtab,
    nrow=2, ncol=1)
  
  return(newplot)
}



points_layer <- function(data, jitter=FALSE, color_runs=FALSE) {
    # Add jitter?
    jit <- ifelse(jitter, 0.3, 0)
    
    # Color by when the plate was run?
    if (color.runs) clr <- quote(RunOrder) else clr <- NULL
    
    points_aes <- aes_q(x=quote(MfgPlate), y=quote(A450), color=clr)

    geom_point(points_aes, data=data,
               size=1, position=position_jitter(jit))
}



hilight_layer <- function(xrange, yrange, interval=25) {
  stopifnot(xrange[[1]] <= xrange[[2]] && yrange[[1]] <= yrange[[2]])
  
  firstdiv <- (xrange[[1]] %/% interval + 1) * interval
  lastdiv <- (xrange[[2]] %/% interval) * interval
  
  if (lastdiv < firstdiv) {
    # The case where there are no intervals inbetween
    divisions <- xrange
  } else {
    # There's at least one division inside
    divisions <- c(xrange[[1]], 
                   seq(firstdiv, lastdiv, by=interval),
                   xrange[[2]])
  }
  
  n <- length(divisions) - 1
  xmin <- divisions[seq(1, length.out=n)] - 0.5
  xmax <- divisions[seq(2, length.out=n)] + 0.5
  
  # Alternate the colors of the regions
  fill <- as.factor(ifelse((divisions[seq_len(n)] %/% 50) %% 2 == 0, 1L, 2L))
  
  df <- data.frame(division=seq_len(n),
                   xmin=xmin, xmax=xmax, 
                   ymin=yrange[[1]], ymax=yrange[[2]],
                   fill=fill)
  
  geom_rect(aes(xmin=xmin, xmax=xmax,
                  ymin=ymin, ymax=ymax,
                fill=fill), data=df, alpha=0.3)
}



test_mfg_plot <- function(...) {
  mfg_table <- read.csv("../Data tables/MfgMastertable.csv")
  plot_mfg(mfg_table, ...)
}
