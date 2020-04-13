#' @title Print Tree Structure of a Fitted 'tree' Object
#' @description Prints the structure of a \code{tree} object, including split
#' variables, split values, predictions and class frequencies.
#' @param x Fitted object of class \code{tree}.
#' @param ... Additional arguments passed to or from other methods.
#' @details Supports classification trees only! Generic \code{print(x, ...)}
#' function for class \code{tree}, so that this method will be dispatched if
#' \code{print(x, ...)} is passed an object of class \code{tree}.
#' Basically a wrapper for \code{print.node()}, as this function only prints
#' the root's response class frequencies and then process to the traversal
#' printing of \code{print.node()} starting at the root.
#' @export
#' @examples
#' # Sample split
#' data <- iris
#' set.seed(1337)
#' rows <- sample.int(nrow(data), size = floor(0.7*nrow(data)), replace = FALSE)
#' trainData <- data[rows,]
#' testData <- data[-rows,]
#' # Create fitted 'tree' object
#' fit <- tree(Species ~ .,
#'             data = trainData,
#'             m = 4,
#'             minsplit = 20,
#'             minbucket = 5,
#'             maxdepth = 3)
#' # Print tree structure
#' fit
#' # Alternatively
#' print(fit)
print.tree <- function(x, ...) {
  # Print root's response class frequencies
  cat("Root frequencies: ",
      parenthesize.table(x$frequencies, type = "counts"),
      " of classes ",
      parenthesize.table(x$frequencies, type = "names"),
      "\n", sep = "")

  # Call recursive tree traversal
  print.node(x$root)
}
