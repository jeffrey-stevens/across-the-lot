# Generate a hierarchy of Day N -- Shift folders:

make_folders <- function(path="../Data files") {
  for (fday in paste("Day", 1:15)) {
    fshift_day <- file.path(path, fday, "Day")
    fshift_eve <- file.path(path, fday, "Evening")
    if (!file.exists(fshift_day)) {
      dir.create(fshift_day, recursive=TRUE)
    } else {
      warning(paste(fshift_day, "already exists."))
    }
    if (!file.exists(fshift_eve)) {
      dir.create(fshift_eve, recursive=TRUE)
    } else {
      warning(paste(fshift_eve, "already exists."))
    }
  }  
}
# Works!


# Generate the random-run-order files
randomize_runs <- function() {
  # This will generate the same randomization each time...
  # Note that Day 1 is different!
  set.seed(525)
  basedir <- "../Plate maps/Runs randomization"
  f_day <- file.path(basedir, paste0("Day ", 1:15, ", Day shift.csv"))
  f_eve <- file.path(basedir, paste0("Day ", 1:15, ", Evening shift.csv"))
  # Day runs
  for (f in f_day) {
    if (!file.exists(f)) {
      df <- data.frame(Run=sample(1:14), MfgPlate=rep("", 14))
      write.csv(df, f, row.names=FALSE)
    } else {
      warning(paste(f, "already exists"))
    }
  }
  # Evening runs
  for (f in f_eve) {
    if (!file.exists(f)) {
      df <- data.frame(Run=sample(2:31), MfgPlate=rep("", 30))
      write.csv(df, f, row.names=FALSE)
    } else {
      warning(paste(f, "already exists"))
    }
  }
}


# Provide extra 15-plate randomization schemes, starting with Day 16
randomize_runs_2 <- function() {
  set.seed(451)
  basedir <- "../Plate maps/Runs randomization"
  f_day <- file.path(basedir, paste0("Day ", 16:21, ", Day shift.csv"))
  f_eve <- file.path(basedir, paste0("Day ", 16:21, ", Evening shift.csv"))
  # Day runs
  for (f in c(f_day, f_eve)) {
    if (!file.exists(f)) {
      df <- data.frame(Run=sample(1:14), MfgPlate=rep("", 14))
      write.csv(df, f, row.names=FALSE)
    } else {
      warning(paste(f, "already exists"))
    }
  }
}

