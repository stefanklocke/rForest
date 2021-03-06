% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMode.R
\name{getMode}
\alias{getMode}
\title{Mode}
\usage{
getMode(x)
}
\arguments{
\item{x}{Vector.}
}
\description{
Calculate most frequent value of some vector \code{x}.
}
\details{
In decision tree analysis with categorial response, mode can be used
as a response variable's prediction value. Functional principle: Considers
all manifestations (unique values) of x just once. First retrieves the
position of the first appearence for each (unique) values inside of \code{x}
using \code{match()}. \code{tabulate()} counts the number of appearences of
each position (value). \code{which.max()} yields the position appearing the
most. Finally, the value at this position inside \code{x} will be returned.
}
\examples{
getMode(c("Apple", "Pear", "Cherry", "Apple"))
}
