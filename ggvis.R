library(ggvis)
library(dplyr)


plot_mfg_ggvis <- function(mfg_table, xrange=NULL, yrange=NULL,
                           brushHandler=function(...) invisible(),
                           coloring="byrun") {
  # coloring:  c("none", "byrun", "bywell")
  
  if ( is.null(xrange) || is.na(xrange) ) {
    xrange <- range(mfg_table$MfgPlate)
  }
  if ( is.null(yrange) || is.na(yrange) ) {
    yrange <- c(0, max(mfg_table$A450))
  }
  
  # Get run coloring:
  if ( is.null(coloring) || is.na(coloring) || coloring == "none") {
    fill_q <- ~ 1  # There must be a better way of doing this...
  } else if ( coloring == "byrun" ) {
    fill_q <- ~ RunOrder
  } else if ( coloring == "bywell" ) {
    fill_q <- ~ WellOrder
  }
  
  hoverHandler <- function(data) {
    id <- data$WellID  # Should drop the data frame...
    plate <- data$MfgPlate
    well <- mfg_table[id, "Well", drop=TRUE]  # Should be unique...
    od <- data$A450
    # Now construct the tooltip:
     out <-
      tags$div(
        plate, br(), well, br(), od,
        style="text-align:center; font-size:70%"
      )
    return(as.character(out))
  }
  
  p <-
    mfg_table %>%
    #mutate(ID=seq_len(nrow(mfg_table))) %>%
    filter( MfgPlate >= xrange[[1]] & MfgPlate <= xrange[[2]] ) %>%
    filter( A450 >= yrange[[1]] & A450 <= yrange[[2]]) %>%
    ggvis( x = ~MfgPlate, y = ~A450, fill = fill_q,
           key := ~WellID) %>%
    layer_points(size:=2, size.brush := 20) %>%
    hide_legend("fill") %>%
    set_options(renderer="canvas") %>%
    add_tooltip(html=hoverHandler, on="hover") %>%
    handle_brush(on_move=brushHandler)
  
  return(p) 
}



filter_by_id <- function( mfg_table, ids ) {
  sel_df <-
    mfg_table %>%
    filter( WellID %in% ids ) %>%
    arrange( WellID ) %>%
    select(MfgPlate, Well, Day, Shift, Run, A450, A650)
  
  return(sel_df)
}


sample_handler <- function(items, session, page_loc, plot_loc) {
  if ( is.null(items) || nrow(items) == 0 ) return ()
  
  sel_df <- filter_by_id( mfg_table, items$WellID )
  show(sel_df)
}


# plot_mfg_reactive <- function(mfg_table, outputVar, ...) {
#   
#   # Doing it this way ensures that the well IDs line up...
#   do_brush <- function(items, session, page_loc, plot_loc) {
#     if ( is.null(items) || nrow(items) == 0 ) return ()
#     
#     sel_df <- filter_by_id( items$WellID )
#     show(sel_df)
#     outputVar$mfg.active.data <- renderDataTable( sel_df )
#   }
#   
#   p <- plot_mfg_ggvis(mfg_table, brushHandler=do_brush, ...)
#   
#   return(p)
# }