source("R/parse-wells-expr.R")

library(plyr)
library(dplyr)
library(ggplot2)


plot_groups <- function(mfg_table, wells) {
  
  groups <- parse_wells_expr(wells)
  
  if (!is.null(groups$grouped)) {
    grouped <- 
      ldply(seq_along(groups$grouped),
            function(i) {
              id <- i
              gr <- groups$grouped[[i]]
              df <- mfg_table %>%
                inner_join(gr$df, by=c(AssayRow="Row", AssayCol="Column")) %>%
                mutate(Group=gr$descr) %>%
                group_by(MfgPlate, Group) %>%
                dplyr::summarise(A450=mean(A450))
              
              return(df)
            })    
  } else {
    grouped <- NULL
  }

  
  if (!is.null(groups$ungrouped)) {
    ungrouped <-
      mfg_table %>%
      inner_join(groups$ungrouped,
                 by=c(AssayRow="Row", AssayCol="Column")) %>%
      mutate(Group=paste0(LETTERS[AssayRow], AssayCol)) %>%
      select(MfgPlate, A450, Group)    
  } else {
    ungrouped <- NULL
  }

  
  # Now merge the two:
  
  grouped_means <- rbind(grouped, ungrouped)
  
  # Now plot
  p <- ggplot(aes(MfgPlate, A450), data=grouped_means) +
    theme_bw() + geom_line(aes(color=Group)) +
    theme(legend.position="top")
  
  return(p)
}

# This works!!!