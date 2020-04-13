#' @title Tree Traversal
#' @description Recursive traversal for reading and editing information of
#' trees.
#' @param object Some tree-like object.
#' @param ... Additional arguments.
#' @details Defined as generic function: \code{traverse(object, ...)} can
#' be added as method for other classes.
#' @rdname Traverse
#' @export
traverse <- function(object, ...) UseMethod("traverse")

#' @title Tree Traversal (Split Rule Extraction)
#' @description Recursively traverses through a classification tree in order to
#' extract information on split rules at each tree node.
#' @param object Object of class \code{node}, in most cases the root node of a
#' \code{tree} object (see examples).
#' @param ans Answer variable, left unspecified by the user. Used internally to
#' return extracted split rules (and to pass them to the next recursive
#' function call).
#' @param ... Additional arguments.
#' @details \code{traverse.node(object)} and \code{traverse(object)} can both
#' be used. In the latter case, \code{traverse.node(object)} will be dispatched
#' if \code{object} is of class \code{node}.
#' @return data.frame of split rules from a classification tree.
#' @method traverse node
#' @export
#' @examples
#' # Create fitted 'tree' object
#' fit <- tree(Species ~ .,
#'             data = iris,
#'             m = 4,
#'             minsplit = 20,
#'             minbucket = 5,
#'             maxdepth = 3)
#' # Traverse to retrieve split rules
#' traverse(fit$root)
traverse.node <- function(object, ans = NULL, ...) {
  # Return, if leaf is reached
  if(class(object) == "leaf") {
    ans
  } else {
    ans <- rbind(ans, as.data.frame(object$rule, stringsAsFactors = FALSE))
    ans <- traverse.node(object$left, ans)
    traverse.node(object$right, ans)
  }
}
