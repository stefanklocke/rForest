#' @title Gini Index
#' @description Calculate the purity of some \code{data} with respect to
#' variable \code{var}.
#' @param data List or data.frame of data.
#' @param var Variable name over which Gini index will be calculated.
#' @return Numeric from [0;1], where 0 indicates fully homogeneous data, 1
#' indicates completely heterogeneous data. Calculation: Retrieves the relative
#' frequencies/probabilities of all classes of \code{var} and returns the gini
#' as 1 minus the sum over all squared relative frequencies.
#' @details Can be used to determine the purity of a decision tree node.
#' @export
#' @examples
#' gini(iris, "Species")
gini <- function(data, var) {
  probs <- table(data[[var]])/length(data[[var]])
  index <- 1 - sum(probs^2)
  index
}
