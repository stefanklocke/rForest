#' @title Information Gain
#' @description Calculate the information gain from binary splitting data into
#' \code{left} and \code{right} groups, based on a pre-split \code{gini} index.
#' @param left data.frame, group of observations resulting from a binary split.
#' @param right data.frame, group of observations resulting from a binary split.
#' @param var Variable over which the information gain will be calculated.
#' @param gini Gini value before split.
#' @return Returns the pre-split \code{gini} index minus the weighted sum of
#' Gini indices of post-split \code{left} and \code{right} groups. Weights are
#' the relative sizes of both left/right groups. Takes values from [0;1], where
#' values greater than 0 indicate an increase in node purity.
#' @export
#' @examples purity <- gini(iris, "Species")
#' gain(iris[1:89,], iris[90:nrow(iris),], "Species", purity)
gain <- function(left, right, var, gini) {
  p <- nrow(left) / (nrow(left) + nrow(right))
  gini - p * gini(left, var) - (1 - p) * gini(right, var)
}
