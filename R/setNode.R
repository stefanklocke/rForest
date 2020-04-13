#' @title Set Classifcation Tree Node
#' @description For internal use in the tree growing function \code{grow()}.
#' A classification tree node is set, if further splits are possible and
#' attractive (next split yields information gain).
#' @param rule Best possible binary split rule for this node.
#' @param left Left group of observations resulting from the node's split.
#' @param right Right group of observations resulting from the node's split.
#' @param response Response variable as specified in the call of \code{tree()}.
#' @param depth Tree level, at which the node is set.
#' @details In the tree growing process, a group of observations will be
#' assigned a node, if the following four conditions are met: 1) Enough
#' observations to further split, 2) the next split will result in both
#' left and right groups to be large enough, 3) the tree's specified maxdepth is
#' not reached and 4) the next best split yields information gain.
#' @return Object of class \code{node}, containing the best possible binary
#' split \code{rule}, the node's \code{depth} as well as \code{left} and
#' \code{right} groups of observations.
#' @export
setNode <- function(rule, left, right, response, depth = 0) {
  node <- list(rule = rule,
               depth = depth,
               left = left,
               right = right,
               names = list(response = response))
  class(node) <- "node"
  node
}
