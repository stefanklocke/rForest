#' @title Summarize a Fitted 'forest' Object
#' @description Prints details information on an object of class \code{forest}.
#' @param object Fitted object of class \code{forest}.
#' @param test data.frame of testing data. Default \code{NULL}, in which case
#' not prediction will be performed and related details will not be printed.
#' If the function is passed some testing data, a confusion matrix and a
#' corresponding classification error rate will be printed additionally.
#' Version note: As of now only Supports classification trees!
#' @param ... Additional arguments passed to or from other methods.
#' @details Generic \code{summary(object, ...)} function for class \code{forest}
#' , so that this method will be dispatched if \code{summary(object, ...)} is
#' passed an object of class \code{forest}.
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
#' # Summarize 'forest' object
#' summary(fit)
summary.forest <- function(object, test = NULL, ...) {
  cat("Call:\n")
  print(object$call)
  cat("\n")

  n <- sum(object$frequencies)
  cat("Number of observations: n =", n, "\n")

  cat("Training data response class distribution:\n")
  print(object$frequencies)
  cat("\n")

  cat("Mean relative bootstrap response class distribution:\n")
  print(object$frequenciesBsRel)
  cat("\n")

  cat("Random predictor subset:", object$control$random, "\n")
  cat("  Predictors (model spec.):",
        length(object$names$predictorsCall), "\n")
  print(object$names$predictorsCall)
  cat("\n")

  cat("Predictor variable importance:\n")
  print(object$importance)
  cat("\n")

  if(is.data.frame(test)) {
    if(nrow(test) == 0) stop("Passed test dataset is empty!")
    pred <- predict(object, test, type = "aggr")
    obs <- test[[object$names$response]]
    tbl <- table(pred, obs)
    cat("Prediction confusion matrix:\n")
    print(tbl)
    cat("\n",
        "Misclassification error rate: ",
        misClassErr(pred, obs)*100, "%", sep = "")
  }
}
