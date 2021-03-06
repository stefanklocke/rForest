% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.forest.R
\name{print.forest}
\alias{print.forest}
\title{Print Information on a Fitted 'forest' Object}
\usage{
\method{print}{forest}(x, ...)
}
\arguments{
\item{x}{Fitted object of class \code{forest}.}

\item{...}{Additional arguments passed to or from other methods.}
}
\description{
Prints basic information on an object of class \code{forest}.
}
\details{
Generic \code{print(x, ...)} function for class \code{forest}, so
that this method will be dispatched if \code{print(x, ...)} is passed an
object of class \code{forest}.
}
\examples{
# Sample split
data <- iris
set.seed(1337)
rows <- sample.int(nrow(data), size = floor(0.7*nrow(data)), replace = FALSE)
trainData <- data[rows,]
testData <- data[-rows,]
# Create fitted 'forest' object
fit <- forest(Species ~ .,
              data = trainData,
              m = 3,
              b = 5,
              minsplit = 20,
              minbucket = 5,
              maxdepth = 3,
              bsinclude = FALSE)
# Print 'forest' object
print(fit)
}
