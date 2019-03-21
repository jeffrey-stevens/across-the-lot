# TODO: Add comment
#
# Author: jstevens
###############################################################################


#' @import grid
NULL


## Specify SBS dimensions

# All measurements are in mm
SBS <- list(
		length=127.76,
		width=85.48,
    cornerRad=3.18,
		marginLength=0,
		hOffset=14.38,
		vOffset=11.24,
		wellSpacing=9.0,
		wellRadius=3.54,
		rowLabelOffset=6,
		colLabelOffset=4
		)



# The default color for the wells
#BASE_WELL_COLOR <- rgb(0.3, 0.3, 1)
#BASE_WELL_COLOR <- "#333366"
BASE_WELL_COLOR <- rgb(0.3, 1, 0.3)



get_color <- function(value, palette="Blues") {
	# value: Vector: 0 to 1
	colfun <- colorRamp(RColorBrewer::brewer.pal(9, palette), space="rgb")
	rgbcol <- colfun(value)
	red <- rgbcol[, 1, drop=TRUE]/255
	green <- rgbcol[, 2, drop=TRUE]/255
	blue <- rgbcol[, 3, drop=TRUE]/255

	return(rgb(red=red, green=green, blue=blue))
}



# It's probably more natural to use hcl colors...

AdjustAlpha <- function(color, alpha) {
	# color: rgb output
	rgbcol <- col2rgb(color)
	red <- rgbcol[[1]]/255
	green <- rgbcol[[2]]/255
	blue <- rgbcol[[3]]/255
	rgb(red=red, green=green, blue=blue, alpha=alpha)
}



build_plate <- function(rowLabels=NULL, colLabels=NULL) {

  # Should check that the dimension of the ODs matrix is right

  plateLength <- SBS$length + SBS$marginLength
  vp <- viewport(height=SBS$width,
                 width=plateLength,
                 default.units='mm')

  pushViewport(vp)

  # Draw the plate frame
  frame_grob <-
    roundrectGrob(x=0.5, y=0.5, height=1, width=1, default.units='npc',
                  r=unit(SBS$cornerRad, "mm"),
                  name="plate frame", vp=vp)

  # Add row and column letters
  rows_list <-
    do.call(gList,
            lapply(1:8,
                   function(row) {
                     yOffset <- SBS$width - SBS$vOffset
                     x <- SBS$rowLabelOffset
                     y <- yOffset - SBS$wellSpacing*(row-1)
                     rowLetter <- LETTERS[[row]]

                     textGrob(rowLetter, x=x, y=y,
                              default.units='mm', vp=vp)
                   }))
  rows_grob <-
    gTree(name="row labels", children=rows_list)

  cols_list <-
    do.call(gList,
            lapply(1:12,
                   function(col) {
                     x <- SBS$hOffset + SBS$wellSpacing*(col-1)
                     y <- SBS$width - SBS$colLabelOffset
                     colNumber <- col

                     textGrob(colNumber, x=x, y=y,
                              default.units='mm', vp=vp, gp=gpar(cex=0.90))
                   }))
  cols_grob <-
    gTree(name="column labels", children=cols_list)


  # Draw the wells
  wells_grid <- expand.grid(Row=1:8, Column=1:12)
  wells_glist <-
    do.call(gList,
            lapply(seq_len(nrow(wells_grid)),
                   function(idx) {
                     row <- wells_grid[idx, "Row"]
                     col <- wells_grid[idx, "Column"]

                     x <- SBS$hOffset + SBS$wellSpacing*(col-1)
                     y <- SBS$width - SBS$vOffset - SBS$wellSpacing*(row-1)

                     wellname <- paste0("well ", row, "-", col)
                     circleGrob(x=x, y=y, r=SBS$wellRadius, name=wellname,
                                default.units='mm')
                   })
    )
  wells_grob <- gTree(name="wells", children=wells_glist)


  # Add the strip labels
  if (identical(rowLabels, NULL)){
    strip_lab_grob <- grob()
  } else {
    strip_lab_grob <-
      do.call(gList,
              lapply(1:8,
                     function(row) {
                       x <- ((SBS$hOffset + SBS$wellSpacing*11 + SBS$wellRadius) +
                               plateLength)/2
                       y <- SBS$width - SBS$vOffset - SBS$wellSpacing*(row-1)

                       if (!is.null(rowLabels)) {
                         rowLab <- rowLabels[[row]]
                       } else {
                         rowLab <- "   "
                       }

                       textGrob(rowLab, x=x, y=y,
                                default.units="mm", vp=vp, gp=gpar(cex=0.75))
                     }
              )
      )
  }

  plate_grob <-
    gTree(name="plate", vp=vp,
          children=gList(frame_grob,
                         rows_grob, cols_grob,
                         wells_grob, strip_lab_grob))

  return(plate_grob)
}



color_wells <- function(plateGrob, rows=1, cols=1, colors="#FFFFFF") {
  stopifnot(identical(length(rows), length(cols)) &&
              identical(length(rows), length(colors)))

  n <- length(rows)
  for (i in seq_len(n)) {
    row <- rows[[i]]
    col <- cols[[i]]
    f <- colors[[i]]

    wellname <- paste0("well ", row, "-", col)
    p <- gPath("wells", wellname)

    plateGrob <-
      editGrob(plateGrob, gPath=p,
               gp=gpar(fill=f))
  }

  return(plateGrob)
}



scale_OD <- function(minod, maxod, value) {
  if (identical(minod, maxod)) {
    stop("Min OD = Max OD.")
  }
  # value:  Can be a vector
  value1 <- ifelse(value < minod, minod, value)
  value2 <- ifelse(value1 > maxod, maxod, value1)
  value3 <- (value2 - minod)/(maxod - minod)

  return(value3)
}



color_plate <- function(rows, cols, ODs, minod=NULL, maxod=NULL,
                        palette=NULL) {
  # df:  Rows, Columns, ODs
  if (is.null(minod)) minod <- 0
  if (is.null(maxod)) maxod <- max(ODs)
  if (is.null(palette)) palette <- "Blues"

  # Draw plate
  plate_grob <- build_plate()

  # Get colors
  ODs_scaled <- scale_OD(minod, maxod, ODs)
  colors <- get_color(ODs_scaled, palette)

  # Color in the wells
  plate_grob <- color_wells(plate_grob, rows, cols, colors)

  return(plate_grob)
}
