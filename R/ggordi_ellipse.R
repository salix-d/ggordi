#' Draw a stat_ellipse path on plot
#' @param g                       ggplot2 object returned from ggordi_base() / ggordi_sites() / ggordi_species
#' @param ellipse.group           one of the ellipse.groups; either 'fill', 'color' or 'shape'; group to draw ellipses for
#' @param ellipse.level           the confidence level at which to draw an ellipse (default is 0.95), or, if type="euclid", the radius of the circle to be drawn.
#' @param ellipse.show.legend     logical, Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param ellipse.type            what type of stat_ellipse to draw : 't', 'norm' or 'euclid'
#' @param ellipse.size            width of the path of the ellipse drawn (size aes of stat_ellipse)
#' @param ellipse.alpha           alpha transparency value for the fill aes of the ellipse
#' @param sites.pal               optional color/fill palette for sites
#' @param lty.pal                 optional palette for line type of ellipse
#' @return A ggplot2 object
#' @export
ggordi_ellipse = function(g, ellipse.group, ellipse.show.legend, ellipse.type, ellipse.level, ellipse.size, sites.pal, lty.pal){
  g <- g + new_scale_color() +
    stat_ellipse(aes(group = ellipse.group, color = ellipse.group, lty = ellipse.group),
                 show.legend = ellipse.show.legend,
                 type = ellipse.type,
                 level = ellipse.level,
                 size = ellipse.size) +
    scale_color_manual(values = setNames(sites.pal, levels(ellipse.group)))
  if(!is.null(lty.pal)){
    g <- g + scale_linetype_manual(values = lty.pal)
  }
  return(g)
}
