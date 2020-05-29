#' ggordi
#'
#' Make an ordination plot using ggplot2
#'
#' @param pcobj                   an object returned by prcomp (will add others later)
#' @param choices                 which PCs to plot
#' @param scale                   covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' scores/sites args
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
#' @param sites.alpha             alpha transparency value for the points (0 = transparent, 1 = opaque)
#' species/loadings args
#' @param draw.arrows             logical,draw arrows for species?
#' @param arrow.color             color of those arrows
#' @param species.geom            either 'point' or 'text'
#' @param species.labs            labels for species
#' @param species.labs.size       label size for species
#' @param species.labs.adjust     adjust ddistance between label and arrow
#' @param species.labs.angled     logical, should the label text be at the same angle as the arrow?
#' ellipse args
#' @param ellipse                 logical, draw ellipse?
#' @param ellipse.groups          any of 'fill', 'color' or 'shape'; group(s) to draw ellipses for
#' @param ellipse.labs            logical, add labels for the ellipse? labels will be factors of groups
#' @param ellipse.level           the confidence level at which to draw an ellipse (default is 0.95), or, if type="euclid", the radius of the circle to be drawn.
#' @param ellipse.show.legend     logical, Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param ellipse.type            what type of stat_ellipse to draw : 't', 'norm' or 'euclid'
#' @param ellipse.size            width of the path of the ellipse drawn (size aes of stat_ellipse)
#' @param ellipse.alpha           alpha transparency value for the fill aes of the ellipse
#' @param lty.pal                 optional palette for line type of ellipse
#' plot args
#' @param axis.lang               language for axes, either 'FR' or 'EN' (for now if not 'FR' assumes it's 'EN')
#' @param draw.axis               logical, draw the 0 axis on the plot?
#' @return A ggplot2 object
#' @export
#'
ggordi = function(pcobj, choices = 1:2, scale = 1,
                  sites.geom = 'point',
                  sites.shape = NULL,
                  sites.leg.title.s = NULL,
                  sites.fill = NULL,
                  sites.leg.title.f = NULL,
                  sites.color = NULL,
                  sites.leg.title.c = NULL,
                  sites.labs = NULL,
                  sites.pal =  NULL,
                  sites.shape.type = 'fill',
                  sites.size = NULL,
                  sites.alpha = 0.25,

                  draw.arrows = T,
                  arrow.color = rgb(0,0,0,0.7),
                  species.labs = NULL,
                  species.geom = 'text',
                  species.labs.size = 4,
                  species.labs.adjust = 0.6,
                  species.labs.angled = T,

                  ellipse.groups = NULL,
                  ellipse = T,
                  ellipse.labs = T,
                  ellipse.level = 0.69,
                  ellipse.show.legend = NA,
                  ellipse.type = 'norm',
                  ellipse.size = 1,
                  ellipse.alpha = 0.15,
                  lty.pal =NULL,

                  axis.lang = 'EN',
                  draw.axis = T
){
  scores = pcobj$x
  exp_var = summary(pcobj)$importance[2,choices]
  n = nrow(scores)
  scaling = pcobj$sdev[choices] * sqrt(n)

  #score data.frame
  x = as.data.frame(t(t(scores[,choices]) / scaling))
  x$shape = sites.shape
  x$fill = sites.fill
  x$color = sites.color
  x$size = if(!is.null(sites.size)) sites.size else 3
  x$label = if(!is.null(sites.labs)) sites.labs else as.character(dimnames(x)[[1L]])

  #species data.frame
  y = as.data.frame(t(t(PCA$rotation[,choices]) * scaling))
  names(y) = c('xend', 'yend')
  y$x = 0
  y$y = 0
  y$label = if(!is.null(species.labs)) species.labs else as.character(dimnames(y)[[1L]])

  #plot axis labels
  axis.labels = setNames(as.list(if(axis.lang == 'FR') paste('CP', choices, sprintf('(%0.1f%%', 100 * exp_var),'var. expliquée)') else  paste('PC', choices, sprintf('(%0.1f%%', 100 * exp_var),'explained var.)')), c('x','y'))

  #get axis ranges
  rangx1 = unsigned.range(x[, 1L])
  rangx2 = unsigned.range(x[, 2L])
  rangy1 = unsigned.range(y[, 1L])
  rangy2 = unsigned.range(y[, 2L])
  xlim = ylim = rangx1 = rangx2 = range(rangx1, rangx2)

  #get ratio to scale arrows
  ratio = max(rangy1/rangx1, rangy2/rangx2)
  y$xend2 = y$xend / ratio * 0.8
  y$yend2 = y$yend / ratio * 0.8
  y$hjust = with(y, (1 - species.labs.adjust * sign(xend2)) / 2)
  y$angle <-if(species.labs.angled) with(y, (180/pi) * atan(yend / xend)) else 0

  if(is.null(sites.leg.title.s)) sites.leg.title.s = as.character(substitute(sites.shape))
  if(is.null(sites.leg.title.f)) sites.leg.title.f = as.character(substitute(sites.fill))
  if(is.null(sites.leg.title.c)) sites.leg.title.c = as.character(substitute(sites.color))

  if(is.null(sites.pal)){
    k = if(!is.null(sites.color)) length(levels(sites.color)) else if(!is.null(sites.fill)) length(levels(sites.fill)) else 1
    sites.pal = get_palette("Set2", k)
    sites.pal.m = get_palette("Dark2", k)
  } else {
    sites.pal.m = muted(sites.pal)
  }

  #draw plot
  g <- ggordi_base(x, draw.axis, axis.labels)
  for(ellipse.group in ellipse.groups){
    ellipse.group = x[,ellipse.group]
    g <- ggordi_ellipse.fill(g, ellipse.group, ellipse.show.legend, ellipse.type, ellipse.level, ellipse.size, ellipse.alpha, sites.pal)
  }
  g <- ggordi_sites(g, x, sites.geom, sites.alpha, sites.shape.type, sites.pal, sites.leg.title.s, sites.leg.title.f, sites.leg.title.c)
  g <- ggordi_species(g, y, draw.arrows, arrow.color, species.labs.size, species.labs.adjust, species.geom, ratio)
  for(ellipse.group in ellipse.groups){
    ellipse.group = x[,ellipse.group]
    g <- ggordi_ellipse(g, ellipse.group, ellipse.show.legend, ellipse.type, ellipse.level, ellipse.size, sites.pal, lty.pal)
    if(ellipse.labs){
      cn = aggregate(setNames(lapply(x[1:2], c), c('x', 'y')), by = setNames(list(ellipse.group), 'label'), mean)
      g <- ggordi_ellipse.labs(g, cn, ellipse.group, sites.pal.m)
    }
  }
  return(g)
}
