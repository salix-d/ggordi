#' Get unsigned range
#' @param x  a vector
#' @return range of vector
#' @export
unsigned.range = function(x) c(-abs(min(x, na.rm = TRUE)),
                               abs(max(x, na.rm = TRUE)))
