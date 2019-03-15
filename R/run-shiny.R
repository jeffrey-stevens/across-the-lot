source("R/global.R")

source("R/stats.R")

source("R/plots/mfg-plot.R")
source("R/plots/plots/runs-plot.R")
source("R/plots/msa-plot.R")
source("R/gradient.R")
source("R/plots/ggvis.R")
source("R/plate-visualization.R")

source("R/ShinyApp/server.R")
source("R/ShinyApp/ui.R")

library(shiny)

options(shiny.trace=FALSE, shiny.error=browser)


readings <- get_readings(factorize = TRUE)
runsmap <- get_runs_map(factorize = TRUE)
msa_table <- get_msa_table()
mfg_table <- get_mfg_table()
mfg_summary <- get_mfg_summary()

days <- max(readings$Day)
mfg_min <- min(mfg_table$MfgPlate)
mfg_max <- max(mfg_table$MfgPlate)
ymax450 <- max(mfg_table$A450)
ymax650 <- max(mfg_table$A650)

runApp(list(ui=build_ui(no.days=days, mfg.min=mfg_min, mfg.max=mfg_max,
                        ymax450=ymax450, ymax650=ymax650),
            server=build_server(readings, runsmap, msa_table, mfg_table,
                                mfg_summary)),
       port=5109, 
       launch.browser=interactive())
