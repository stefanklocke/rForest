#' @title Misclassification Error Rate
#' @description Calculates the percentage of falsely predicted values.
#' @param predicted Vector of prediction values.
#' @param observed Vector of observed values.
#' @export
#' @examples
#' data <- iris
#' set.seed(1337)
#' rows <- sample.int(nrow(data), size = floor(0.7*nrow(data)), replace = FALSE)
#' trainData <- data[rows,]
#' testData <- data[-rows,]
#' fit <- tree(Species ~ .,
#'             data = trainData,
#'             m = 2,
#'             minsplit = 20,
#'             minbucket = 5,
#'             maxdepth = 3)
#' pred <- predict(fit, testData)
#' obs <- testData$Species
#' misClassErr(pred, obs)
misClassErr <- function(predicted, observed) {
  # Confusion matrix
  tbl <- table(predicted, observed)
  # Error rate: Sum over correctly predicted (diagonale) div. by sum over all
  err <- 1 - sum(diag(tbl))/sum(tbl)
  err
}
