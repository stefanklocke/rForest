#' @title Summarize a Fitted 'tree' Object
#' @description Prints details information on an object of class \code{tree}.
#' @param object Fitted object of class \code{tree}.
#' @param test data.frame of testing data. Default \code{NULL}, in which case
#' not prediction will be performed and related details will not be printed.
#' If the function is passed some testing data, a confusion matrix and a
#' corresponding classification error rate will be printed additionally.
#' Version note: As of now only Supports classification trees!
#' @param ... Additional arguments passed to or from other methods.
#' @details Generic \code{summary(object, ...)} function for class \code{tree},
#' so that this method will be dispatched if \code{summary(object, ...)} is
#' passed an object of class \code{tree}.
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
#'             m = 2,
#'             minsplit = 20,
#'             minbucket = 5,
#'             maxdepth = 3)
#' # Summarize 'tree' object
#' summary(fit)
summary.tree <- function(object, test = NULL, ...) {
  cat("Call:\n")
  print(object$call)
  cat("\n")

  n <- sum(object$frequencies)
  cat("Number of observations: n =", n, "\n")

  cat("Root response class distribution:\n")
  print(object$frequencies)
  cat("\n")

  cat("Random predictor subset:", object$control$random, "\n")
  cat("  Predictors (model spec.): p =",
        length(object$names$predictorsCall), "\n")
  print(object$names$predictorsCall)
  if(object$control$random) {
    cat("  Predictors (rand. subs.): m =",
        length(object$names$predictors), "\n")
    print(object$names$predictors)
  }
  cat("\n")

  splits <- traverse(object$root)
  cat("Nodes and split rules:\n")
  print(splits)
  cat("\n")

  cat("Predictor variable importance:\n")
  print(object$importance)
  cat("\n")

  if(is.data.frame(test)) {
    if(nrow(test) == 0) stop("Passed test dataset is empty!")
    pred <- predict(object, test)
    obs <- test[[object$names$response]]
    tbl <- table(pred, obs)
    cat("Prediction confusion matrix:\n")
    print(tbl)
    cat("\n",
        "Misclassification error rate: ",
        misClassErr(pred, obs)*100, "%", sep = "")
  }
}
