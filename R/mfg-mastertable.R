source("R/util.R")

library(dplyr)

readings <- read.csv("../Data tables/Readings.csv")
mfg_map <- read.csv("../Plate maps/MfgMap.csv")

# The merger should be easy...
mfg_table_1 <- merge(mfg_map, readings)

# Equate Mfg rows/cols with Assay rows/cols:
rows <- data.frame(MfgRow=1:8, AssayRow=1:8)
cols <- data.frame(MfgCol=1:12, AssayCol=1:12)

mfg_table_2 <-
  mfg_table_1 %>%
  merge(rows) %>%
  merge(cols) %>%
  expand_tab() %>%
  arrange(MfgPlate, AssayRow, AssayCol) %>%  # Set the well ordering...
  mutate(WellID=seq_len(n())) %>%
  select(WellID, MfgPlate, Well, AssayRow, AssayCol, 
         WellOrder, Day, Shift, Run, RunOrder, A450, A650)


# That's all there is to it!
write.csv(mfg_table_2, "../Data tables/MfgMastertable.csv", row.names=FALSE)


### Calculate summary statistics

mfg_summary <- calc_summary(mfg_table_2)

write.csv(mfg_summary, "../Data tables/MfgSummary.csv", row.names=FALSE)
