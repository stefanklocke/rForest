#' @title Binary Split Dataset
#' @description Splits \code{data} with respect to some variable \code{splitVar}
#' and some specified value {splitVal}.
#' @param data data.frame of data to be split.
#' @param splitVar Variable with respect to which \code{data} shall be split.
#' @param splitVal Value of \code{splitVar}, at which \code{data} shall be
#' split.
#' @return List of \code{left} and \code{right} groups of observations.
#' @export
setSplit <- function(data, splitVar, splitVal) {
  # Respect split case: Numeric (left if <), categorial (left if !=)
  if(is.numeric(data[[splitVar]])) {
    groups <- list(left = data[data[[splitVar]] < splitVal, ],
                   right = data[data[[splitVar]] >= splitVal, ])
  } else {
    groups <- list(left = data[data[[splitVar]] != splitVal, ],
                   right = data[data[[splitVar]] == splitVal, ])
  }
  groups
}
