#' @title Mode
#' @description Calculate most frequent value of some vector \code{x}.
#' @param x Vector.
#' @details In decision tree analysis with categorial response, mode can be used
#' as a response variable's prediction value. Functional principle: Considers
#' all manifestations (unique values) of x just once. First retrieves the
#' position of the first appearence for each (unique) values inside of \code{x}
#' using \code{match()}. \code{tabulate()} counts the number of appearences of
#' each position (value). \code{which.max()} yields the position appearing the
#' most. Finally, the value at this position inside \code{x} will be returned.
#' @export
#' @examples
#' getMode(c("Apple", "Pear", "Cherry", "Apple"))
getMode <- function(x) {
  # See getMode()'s help page for details.
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}
