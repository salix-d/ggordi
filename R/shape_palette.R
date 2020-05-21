#' Get n shape numbers for scale_shape_manual values
#'
#' @param n     how many shape numbers should be return?
#' @param type  the ype of shape to be return : 'solid' (full shape), 'stroke' (empty shapes), 'fill' (full shape with color and fill param) or 'lines' (shapes made with lines(+, *, etc...))
#' @return A vector of pch number
#' @export

shape_palette = function(n, type = 'solid'){
  if (n > 6) {
    msg <- paste("The shape palette can deal with a maximum of 6 discrete ",
                 "values because more than 5 becomes difficult to discriminate; ",
                 "you have ", n, ". Consider specifying shapes manually if you ",
                 "must have them.", sep = "")
    warning(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
  }
  if (type == 'solid') {
    return(c(16, 17, 15, 18, 19)[seq_len(n)])
  }
  if (type == 'stroke') {
    return(c(1,2,0,5,6)[seq_len(n)])
  }
  if (type == 'fill') {
    return(c(21,24,22,23,25)[seq_len(n)])
  }
  if (type == 'lines') {
    return(c(10,12,9,3,4)[seq_len(n)])
  }
  else {
    stop('type must be one of "solid", "stroke", "fill" or "lines"')
  }
}
