#' @title Set Classifcation Tree Leaf
#' @description For internal use in the tree growing function \code{grow()}.
#' A classification tree leaf is set, if no further splits are possible or
#' attractive (no further information gain).
#' @param group Left or right group of observations to which a leaf will be
#' assigned.
#' @param response Response variable as specified in the call of \code{tree()}.
#' @param depth Tree level, at which the leaf is set.
#' @details In the tree growing process, a group of observations will be
#' assigned a leaf if 1) not enough observations to further split, 2) the next
#' split will result in either one or both left/right groups to be too small, 3)
#' the tree's specified maxdepth is reached or 4) if the next best split does
#' not yield information gain.
#' @return Object of class \code{leaf}, containing elements \code{depth}, a
#' group of observations \code{obs}, a response \code{prediction} (based on
#' mode) and \code{predictionErr}, the relative frequency of response classes
#' other than \code{prediction}.
#' @export
setLeaf <- function(group, response, depth = 0) {
  # Set prediction as most frequent response value
  prediction <- getMode(group[[response]])
  # Absolute response class frequencies
  tbl <- table(group[[response]])
  # Error: 1 - relative frequency of predicted class
  predictionErr <- 1 - tbl[[prediction]] / sum(tbl)
  # Assemble leaf
  leaf <- list(depth = depth,
               obs = group,
               prediction = prediction,
               predictionErr = predictionErr,
               names = list(response = response))
  class(leaf) <- "leaf"
  leaf
}
