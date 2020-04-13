#' @title Print Information on a Fitted 'forest' Object
#' @description Prints basic information on an object of class \code{forest}.
#' @param x Fitted object of class \code{forest}.
#' @param ... Additional arguments passed to or from other methods.
#' @details Generic \code{print(x, ...)} function for class \code{forest}, so
#' that this method will be dispatched if \code{print(x, ...)} is passed an
#' object of class \code{forest}.
#' @export
#' @examples
#' # Sample split
#' data <- iris
#' set.seed(1337)
#' rows <- sample.int(nrow(data), size = floor(0.7*nrow(data)), replace = FALSE)
#' trainData <- data[rows,]
#' testData <- data[-rows,]
#' # Create fitted 'forest' object
#' fit <- forest(Species ~ .,
#'               data = trainData,
#'               m = 3,
#'               b = 5,
#'               minsplit = 20,
#'               minbucket = 5,
#'               maxdepth = 3,
#'               bsinclude = FALSE)
#' # Print 'forest' object
#' print(fit)
print.forest <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat("Type of forest: ")
  if(x$control$random) {
    cat("Random Forest")
  } else {
    cat("Bagged tree ensemble")
  }
  cat("\n\n")

  n <- sum(x$frequencies)
  cat("Number of observations: n =", n, "\n")

  b <- length(x$ensemble)
  cat("Number of bootstrap samples and bagged trees: b =", b, "\n")
  cat("\n")

  cat("Predictors (model spec.):",
        length(x$names$predictorsCall), "\n")
  print(x$names$predictorsCall)
  cat("\n")

  cat("Predictor variable importance:\n")
  print(x$importance)
  cat("\n")
}
