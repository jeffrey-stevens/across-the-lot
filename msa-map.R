library(dplyr)

msa_map <- read.csv("../Plate maps/EmbeddedMSAAssemblyMap.csv")
msa_mfg_map <- read.csv("../Plate maps/EmbeddedMSAMfgMap.csv")


msa_map_1 <- 
  msa_map %>%
  inner_join(msa_mfg_map) %>%
  select(MSAPlate, AssayStrip, MfgPlate, Pool, MfgStrip=PoolPlateStrip) %>%
  arrange(MSAPlate, AssayStrip)

write.csv(msa_map_1, "../Plate maps/MSAMap.csv", row.names=FALSE)


# Convert strips to columns:
assay_cols <- data.frame(AssayStrip=rep(1:6, each=2), AssayCol=1:12)

msa_map_2 <- 
  msa_map_1 %>%
  inner_join(assay_cols) %>%
  mutate(MfgCol=ifelse(AssayCol %% 2 == 1,
                       MfgStrip*2-1, MfgStrip*2)) %>%
  merge(data.frame(MfgRow=1:8, AssayRow=1:8), all=TRUE) %>%
  select(MSAPlate, AssayStrip, AssayCol, AssayRow,
         MfgPlate, Pool, MfgStrip, MfgCol, MfgRow) %>%
  arrange(MSAPlate, AssayCol, AssayRow)

write.csv(msa_map_2, "../Plate maps/MSAMastermap.csv", row.names=FALSE)



# Now merge with the readings table

readings <- read.csv("../Data tables/Readings.csv")
msa_runs_map <- read.csv("../Plate maps/MSARunsMap.csv")

msa_mastertable <-
  msa_map_2 %>%
  inner_join(msa_runs_map) %>%
  inner_join(readings) %>%
  select(Day, Shift, Run, MSAPlate, AssayStrip, AssayCol,
         AssayRow, MfgPlate, Pool, MfgStrip, MfgCol, MfgRow,
         A450, A650) %>%
  arrange(Day, Shift, Run)

write.csv(msa_mastertable, "../Data tables/MSAMastertable.csv", row.names=FALSE)