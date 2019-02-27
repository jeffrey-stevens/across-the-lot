filler_mastertable <- read.csv("../Data tables/FillerMastertable.csv")

library(plyr)

filler_stats <-
  ddply(filler_mastertable, .(MfgPlate),
        function(df) {
          c("Mean 450"=mean(df$A450),
            "StDev 450"=sd(df$A450),
            "CV 450"=sd(df$A450)/mean(df$A450),
            "Median 450"=median(df$A450),
            "Q1 450"=quantile(df$A450)[["25%"]],
            "Q3 450"=quantile(df$A450)[["75%"]],
            "Min 450"=min(df$A450),
            "Max 450"=max(df$A450),
            "Mean 650"=mean(df$A650),
            "StDev 650"=sd(df$A650),
            "CV 650"=sd(df$A650)/mean(df$A650),
            "Median 650"=median(df$A650),
            "Q1 650"=quantile(df$A650)[["25%"]],
            "Q3 650"=quantile(df$A650)[["75%"]],
            "Min 650"=min(df$A650),
            "Max 650"=max(df$A650))
        })

library(ggplot2)

mfg_plot <- ggplot() + theme_bw() +
  geom_point(data=filler_mastertable, aes(x=MfgPlate, y=A450),
             size=1.5, color="black", fill="black",
             alpha=0.2) +
#   geom_segment(data=filler_stats,
#             aes(x=MfgPlate-1, xend=MfgPlate+1,
#                 y=filler_stats[["Mean 450"]], yend=filler_stats[["Mean 450"]]),
#             color="red", size=0.5) +
  ylim(0.0, NA) + ggtitle("Lot AL695\n")
  
png("../Plots/MfgPlot.png", width=12, height=4,
       units="in", res=90, type="windows")
mfg_plot
dev.off()
