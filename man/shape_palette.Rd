% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shape_palette.R
\name{shape_palette}
\alias{shape_palette}
\title{Get n shape numbers for scale_shape_manual values}
\usage{
shape_palette(n, type = "solid")
}
\arguments{
\item{n}{how many shape numbers should be return?}

\item{type}{the ype of shape to be return : 'solid' (full shape), 'stroke' (empty shapes), 'fill' (full shape with color and fill param) or 'lines' (shapes made with lines(+, *, etc...))}
}
\value{
A vector of pch number
}
\description{
Get n shape numbers for scale_shape_manual values
}
