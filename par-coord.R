source("select-wells.R")

library(GGally)
library(tidyr)
library(dplyr)

# What about missing plates?  There aren't any gaps...
plot_parcoord <- function(mfg_table,
                          mfg.range=range(mfg_table$MfgPlate),
                          wells="A1-H12") {
  # First, get the wells to include:
  selection <- select_wells(wells)
  
  # Widen the table:
  wide_table <- 
    mfg_table %>%
    select(MfgPlate, AssayRow, AssayCol, A450) %>%
    filter(MfgPlate >= mfg.range[[1]], MfgPlate <= mfg.range[[2]]) %>%
    inner_join(selection, by=c(AssayRow="Row", AssayCol="Column")) %>%
    mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                              paste0(LETTERS[1:8], collapse=""),
                              AssayRow),
                       AssayCol)) %>%
    spread(key=MfgPlate, value=A450)
  
  # What if the range is empty!
  
  if (!identical(nrow(wide_table), 0L)) {
    ymax <- max(mfg_table$A450)
    p <- 
      ggparcoord(wide_table, columns=seq(4, ncol(wide_table)),
                 groupColumn=3, scale="globalminmax",
                 alphaLines=0.3) + theme_bw() + ylim(c(0, ymax))
  } else {
    warning("No wells selected.")
    p <- NULL
  }
  
  return(p)
}


plot_parcoord_2 <- function(mfg_table, 
                            mfg.range=range(mfg_table$MfgPlate),
                            wells="A1-H12") {
  
  # First, get the wells to include:
  selection <- select_wells(wells)
  
  # Widen the table:
  wide_table <- 
    mfg_table %>%
    select(MfgPlate, AssayRow, AssayCol, A450) %>%
    filter(MfgPlate >= mfg.range[[1]], MfgPlate <= mfg.range[[2]]) %>%
    inner_join(selection, by=c(AssayRow="Row", AssayCol="Column")) %>%
    mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                              paste0(LETTERS[1:8], collapse=""),
                              AssayRow),
                       AssayCol)) %>%
    select(everything(), -AssayRow, -AssayCol ) %>%
    spread(key=Well, value=A450)
  
  
  # Need to colorcode
  geoms <- 
    lapply(colnames(wide_table)[2:97],
           function(n) {
             geom_line(aes_string(y=n))
           })
  
  # Should reorder the columns...
  par_plot <-
    Reduce('+',
           geoms,
           init=ggplot(aes(x=MfgPlate), data=wide_table))
  
  return(par_plot)
}


plot_parcoord_3 <- function(mfg_table, 
                            mfg.range=range(mfg_table$MfgPlate),
                            wells="A1-H12",
                            alpha=0.3) {
  
  # First, get the wells to include:
  selection <- select_wells(wells)
  
  # Add well names
  wells <- 
    expand.grid(AssayCol=1:12, AssayRow=1:8) %>%
    mutate(Well=paste0(chartr(paste0(1:8, collapse=""),
                         paste0(LETTERS[1:8], collapse=""),
                         AssayRow),
                          AssayCol),
           WellOrder=as.numeric(seq_len(n())))
  # Well ordering is right!
  
  wells_tab <- 
    mfg_table %>%
    filter(MfgPlate >= mfg.range[[1]], MfgPlate <= mfg.range[[2]]) %>%
    inner_join(selection, by=c(AssayRow="Row", AssayCol="Column")) %>%
    inner_join(wells)
  

  par_plot <-
    ggplot() +
    geom_line(aes(x=MfgPlate, y=A450, color=Well),
              data=wells_tab, alpha=alpha) + theme_bw() +
    theme(legend.position="none")
#     theme(legend.position="top", legend.direction="horizontal") +
#     guides(color=guide_legend(label.position="top"))
# Legends don't seem to be working...The problem is with
# geom_line with an alpha setting...
  
  return(par_plot)
}



