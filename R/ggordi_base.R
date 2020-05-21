#' Draw base plot with no sites or speciesé
#' @param x             scaled scores data.frame with columns for choices (PC1, PC2, etc.) and optional columns for shape, fill, color and label aes
#' @param draw.axis     logical, draw the 0 axis on the plot?
#' @param axis.labels   named list of x and y axis label
#' @return A ggplot2 object
#' @export
ggordi_base = function(x, draw.axis, axis.labels){
  lims = range(unsigned.range(x[, 1L]), unsigned.range(x[, 2L]))
  g <- ggplot(x, aes(x = x[,1L], y = x[,2L])) +
    lims(x = lims, y = lims) + coord_equal() + labs(x = axis.labels$x, y = axis.labels$y) + theme_bw()
  if(draw.axis){
    g <- g +
      geom_hline(yintercept = 0, linetype="dashed") +
      geom_vline(xintercept = 0, linetype="dashed")
  }
  return(g)
}
