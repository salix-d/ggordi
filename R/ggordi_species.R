#' Draw the loadings/speacies on the base plot
#' @param g                       ggplot2 object returned from ggordi_base() / ggordi_sites()
#' @param draw.arrows             logical,draw arrows for species?
#' @param arrow.color             color of those arrows
#' @param species.geom            either 'point' or 'text'
#' @param species.labs.size       label size for species
#' @param species.labs.adjust     adjust ddistance between label and arrow
#' @param ratio                   ratio to which arrows should be scaled
#' @return A ggplot2 object
#' @export
ggordi_species = function(g, y, draw.arrows, arrow.color, species.labs.size, species.labs.adjust, species.geom, ratio){
  if(species.geom == 'text'){
    if(draw.arrows){
      g <- g + geom_segment(data = y, aes(x = x, y = y, xend = xend2, yend = yend2), color = arrow.color,
                            arrow = arrow(length = unit(1/2, 'picas')))
    }
    g <- g +
      geom_text(data = y, aes(x = xend2*1.1, y = yend2*1.1, label = label), color = arrow.color,
                angle = y$angle, hjust = y$hjust, size = species.labs.size) +
      scale_y_continuous(expand = expansion(mult = 0.25/ratio, add = 0)) +
      scale_x_continuous(expand = expansion(mult = 0.25, add = 0))
  }
  if(species.geom == 'point'){
    g <- g + new_scale_color() +
      geom_point(data = y, aes(x = xend2*1.1, y = yend2*1.1, color = label), size = 5)
  }
  return(g)
}
