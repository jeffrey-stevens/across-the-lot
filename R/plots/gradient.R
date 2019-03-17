
plot_gradient <- function(mfg_table, type="points",
                          margin="none") {
  # type = c("points", "rays")
  # margin = c("histogram", "density", "none")

  models <- get_models(mfg_table)

  # Now plot the gradient:

  rng.max <- max(abs(models$AssayRow), abs(models$AssayCol))
  rng <- c(-rng.max, rng.max)

  plot_frame <-
    ggplot(data=models) + theme_bw() +
    xlim(rng) + ylim(rng) +
    xlab("\nColumn\n(OD / well index)") + ylab("\nRow\n(OD / well index)")

  p <-
    switch(type,
      "points" = geom_point(aes(x=AssayCol, y=AssayRow)),
      "rays"  = geom_segment(aes(x=0, xend=AssayCol, y=0, yend=AssayRow),
                            alpha=0.5) )

  main_plot <- plot_frame + p

  if ( is.null(margin) || is.na(margin) ) {
    margin <- "none"
  }

  if (margin=="histogram" | margin=="density") {
    # This appears to be broken...
    full_plot <- ggExtra::ggMarginal(main_plot, x="AssayCol", y="AssayRow",
                             type=margin)
  } else if (margin=="none") {
    full_plot <- main_plot
  } else {
    warning('"margin" must be either "points", "rays" or "none"')
    return(NULL)
  }

  return(full_plot)
}


get_models <- function(mfg_table) {

  models <-
    plyr::ddply(mfg_table, "MfgPlate",
                function(df) {
                  # Center the data, for convenience:
                  df_centered <- data.frame(AssayRow=df$AssayRow-4.5,
                                            AssayCol=df$AssayCol-6.5,
                                            A450=df$A450)
                  mdl <- try({lm(A450 ~ AssayRow + AssayCol, data=df_centered)})
                  if (identical(class(mdl), "lm")) {
                    return(coefficients(mdl))
                  } else {
                    warning("Error fitting the model.")
                    return(NULL)
                  }
                })
  names(models)[[2]] <- "Intercept"

  return(models)
}


# This doesn't work well...
plot_grad_arrows <- function(mfg_table) {
  models <- get_models(mfg_table)
  ggplot(data=models) +
    geom_segment(aes(x=MfgPlate*0.02, xend=MfgPlate*0.02+AssayCol,
                     y=0, yend=AssayRow),
                 alpha=1, arrow=arrow())
}
