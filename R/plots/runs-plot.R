

plot_shift <- function(day=1, shift="Day", wl=450,
                       readings=NULL, runsmap=NULL,
                       yrange=NULL) {
  stopifnot(wl %in% c(450, 650))

  if (is.null(readings)) {
    readings <- get_readings()
  }
  if (is.null(runsmap)) {
    runsmap <- get_runs_map()
  }

  wlcol <- paste0("A", wl)
  if ( is.null(yrange) ) {
    yrange <- c(0, max(readings[, wlcol, drop=TRUE]))  # The *global* maximum
  }

  dat <-
    readings %>%
    select_("Day", "Shift", "Run", wlcol) %>%
    filter( Day == day, Shift == shift) %>%
    inner_join(runsmap,
               by=c("Day"="Day", "Shift"="Shift", "Run"="Run")) %>%
    mutate(Run=as.ordered(Run),
           Label=paste0(Run, "\n", Class, "\n", ID))

  if (nrow(dat)==0) {
    warning("No entries for indicated Day and Shift.")
    return()
  }

  dat_stats <-
    dat %>%
    group_by(Run, Label) %>%
    summarise_(MeanOD=lazyeval::interp(~mean(col), col = as.name(wlcol))) %>%
    arrange(Run)


  p <-
    ggplot() + theme_bw() +
    geom_line(aes(x=Run, y=MeanOD, group=1),
              data=dat_stats,
              color="red2", size=0.75) +
    geom_jitter(
      aes_string(x="Run", y=wlcol),
      data=dat,
      position=position_jitter(width=0.20, height=0),
      shape="o", size=2.5) +
    scale_x_discrete(labels=dat_stats$Label) +
    ylim(yrange) + xlab("\nRun") + ylab(paste0(wlcol, "\n"))

  return(p)
}
