# build-tables.R
#
# Build the "working" tables from the raw data and mapping files.
#
# Note that this will work with either SQL tables or tibbles.


build_shift_order_table <- function(readings) {

  shift_order <-
    readings %>%
    select(Day, Shift) %>%
    unique() %>%
    arrange(Day, Shift) %>%
    mutate(RunOrder=seq_len(n()))

  return(shift_order)
}


# This should work regardless of the data source (SQL database or tibbles).
build_mfg_table <- function(readings, mfg_map, shift_order, factorize = TRUE) {

  # Equate Mfg rows/cols with Assay rows/cols:
  rows <- data.frame(MfgRow=1:8, AssayRow=1:8)
  cols <- data.frame(MfgCol=1:12, AssayCol=1:12)

  well_names <- get_wells(factorize)

  mfg_table <-
    mfg_map %>%
    inner_join(readings, by = c("Day", "Shift", "Run") ) %>%
    inner_join(rows, by = "AssayRow") %>%
    inner_join(cols, by = "AssayCol") %>%
    inner_join(well_names, by = c("AssayRow", "AssayCol")) %>%
    inner_join(shift_order, by = c("Day", "Shift") ) %>%
    arrange(MfgPlate, AssayRow, AssayCol) %>%  # Set the well ordering...
    mutate(WellID=seq_len(n())) %>%
    select(WellID, MfgPlate, Well, AssayRow, AssayCol,
           WellOrder, Day, Shift, Run, RunOrder, A450, A650)

  return(mfg_table)
}


build_summary_table <- function(mfg_table) {

  summary_tab <-
    mfg_table %>%
    group_by(Day, Shift, Run, MfgPlate) %>%
    summarise(Mean450=mean(A450), SD450=sd(A450), CV450=sd(A450)/mean(A450)*100,
              Median450=median(A450), Range450=diff(range(A450)),
              RelRange450=diff(range(A450))/mean(A450)*100,
              Mean650=mean(A650), SD650=sd(A650), CV650=sd(A650)/mean(A650)*100,
              Median650=median(A650), Rang650=diff(range(A650)),
              RelRange650=diff(range(A650))/mean(A650)*100) %>%
    arrange(MfgPlate)

  return(summary_tab)
}


build_msa_map <- function(msa_mfg, msa_assembly) {

  msa_map <-
    msa_mfg %>%
    inner_join(msa_assembly, by = "PoolPlateID" ) %>%
    select(MSAPlate, AssayStrip, MfgPlate, Pool, MfgStrip = PoolPlateStrip) %>%
    arrange(MSAPlate, AssayStrip)

  return(msa_map)
}


build_msa_table <- function(readings, msa_map, msa_runs, factorize = TRUE) {

  well_names <- get_wells(factorize)

  # Convert strips to columns:
  assay_cols <- data.frame(AssayStrip=rep(1:6, each=2), AssayCol=1:12)

  msa_table <-
    msa_map %>%
    inner_join(assay_cols, by = "AssayStrip") %>%
    mutate(MfgCol=ifelse(AssayCol %% 2 == 1,
                         MfgStrip*2-1, MfgStrip*2)) %>%
    merge(data.frame(MfgRow=1:8, AssayRow=1:8), all = TRUE ) %>%
    inner_join(well_names, by = c("AssayRow", "AssayCol")) %>%
    inner_join(msa_runs, by = "MSAPlate") %>%
    inner_join(readings, by = c("Day", "Shift", "Run", "AssayRow",
                                "AssayCol") ) %>%
    select(Day, Shift, Run, MSAPlate, AssayStrip, AssayCol,
           AssayRow, WellOrder, MfgPlate, Pool, MfgStrip, MfgCol, MfgRow,
           A450, A650) %>%
    arrange(Day, Shift, Run, WellOrder)


  return(msa_table)
}
