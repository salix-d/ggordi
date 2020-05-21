#' Draw the scores/sites on the base plot
#' @param g             ggplot2 object returned from ggordi_base()
#' @param sites.geom              either 'point' or 'text'
#' @param sites.shape             shape aes for sites
#' @param sites.leg.title.s       legend title for shape
#' @param sites.fill              fill aes for sites
#' @param sites.leg.title.f       legend title for fill
#' @param sites.color             color aes for sites
#' @param sites.leg.title.c       legend title for color
#' @param sites.labs              optionnal vector oflabels for sites if different than rownames
#' @param sites.pal               optional color/fill palette for sites
#' @param sites.shape.type        type of shape to get for points : 'solid', 'stoke', 'fill' or 'lines' (see function shape_palette for which pch is in which)
#' @param sites.size              optional size for points (not an aes for now)
#' @param sites.alpha              alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @return A ggplot2 object
#' @export
ggordi_sites <- function(g, sites.geom, sites.alpha, sites.shape.type, sites.pal, sites.leg.title.s, sites.leg.title.f, sites.leg.title.c){
  if(sites.geom == 'text') g <- g + geom_text(aes(label = label))
  if(sites.geom == 'point'){
    if(length(unique(x$size)) == 1){
      g <- g + geom_point(aes(shape = shape, fill = fill), size = x$size, alpha = sites.alpha)
    } else {
      g <- g + geom_point(aes(shape = shape, fill = fill, size = size), alpha = sites.alpha)
    }
    if(!is.null(x$shape)){
      g <- g + scale_shape_manual(values = setNames(shape_palette(2, sites.shape.type), levels(x$shape)), name = sites.leg.title.s)
      if(sites.shape.type == 'fill'){
        g <- g + scale_fill_manual(values = setNames(sites.pal, levels(x$fill)), name = sites.leg.title.f)
      } else{
        g <- g + scale_color_manual(values = setNames(sites.pal, levels(x$color)), name = sites.leg.title.c)
      }
      g <- g +
        guides(fill = guide_legend(override.aes = list(size = 4, shape = if(sites.shape.type == 'fill') 21 else 16)),
               shape = guide_legend(override.aes = list(size = 4)))
    }
  }
}
