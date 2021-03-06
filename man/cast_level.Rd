% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cast_level.R
\name{cast_level}
\alias{cast_level}
\title{Convert classification ID to factor a specified level of classification}
\usage{
cast_level(x, meta = attr(x, "classification"), level = length(meta),
  label = TRUE)
}
\arguments{
\item{x}{the vector to cast}

\item{meta}{an oject of type \code{classification}.}

\item{level}{the level of the classificatio to which the variable should be
cast.}

\item{label}{return the labels of the categories. Otherwise return the 
codes.}
}
\description{
Convert classification ID to factor a specified level of classification
}
