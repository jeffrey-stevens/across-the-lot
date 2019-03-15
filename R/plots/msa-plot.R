library(ggplot2)
library(dplyr)

plot_msa <- function(msa_table) {

  # Plot by run order:
  plot_order <- 
    unique(msa_table[,c("Day", "Shift", "Run")]) %>%
    arrange(Day, Shift, Run) %>%
    mutate(PlotOrder=as.factor(seq_len(n())),
           Labels=sprintf("D%0d\n%s\nR%0d",
                Day, substr(Shift, 1, 1), Run))
  
  msa_table_ordered <-
    msa_table %>%
    inner_join(plot_order[,c("Day", "Shift", "Run", "PlotOrder")])
  
  msa_summary <-
    summarise(group_by(msa_table_ordered, PlotOrder),
              MeanOD=mean(A450),
              CV=sd(A450)/mean(A450)) %>%
    mutate(PlotOrder=as.integer(PlotOrder))

  grand_mean <- mean(msa_table$A450)
  
  p <-
    ggplot() + theme_bw() +
    ggtitle("By Run order\n") +
    labs(x="\nMSA plate", y="A450\n") +
    ylim(0, max(msa_table$A450) + 0.02) +
    scale_x_discrete(breaks=plot_order$PlotOrder, labels=plot_order$Labels) +
    geom_abline(intercept=grand_mean, slope=0, color="green", size=1) +
    geom_segment(aes(x=PlotOrder-0.5, y=MeanOD,
                     xend=PlotOrder+0.5, yend=MeanOD),
                 data=msa_summary, color="red", size=1, alpha=0.7) +
    geom_point(aes(x=PlotOrder, y=A450),
      data=msa_table_ordered, position=position_jitter(0.3),
      shape="o", size=2.5)

  
  return(p)
}