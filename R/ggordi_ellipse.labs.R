#' Adds labels to ellipses at their centroids
#'
#' @param g               a ggplot2 object to which ellipse laels will be added
#' @param centroids.df    a data.frame with the centroids coordinates as 'x' and 'y' and the grouping variable as 'label'
#' @param sites.pal       optional color/fill palette for sites
#' @return A ggplot2 object
#' @export

#ellipse label at centroïds
ggordi_ellipse.labs = function(g, centroids.df, ellipse.group, sites.pal){
  g <- g + geom_text(data = centroids.df, aes(x = x, y = y, label = label), color = sites.pal.m, fontface = 'bold')
  return(g)
}
