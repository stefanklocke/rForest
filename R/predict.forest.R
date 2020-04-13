#' @title Prediction for a Fitted 'forest' Object
#' @description Generates a vector of prediction values or a matrix of
#' prediction probabilities for a \code{forest} of fitted \code{tree} objects.
#' @param object \code{forest} object of fitted \code{tree} models.
#' @param data Data containing the response variable to be predicted.
#' @param type Can be either \code{"aggr"} or \code{"prob"}. For \code{"aggr"},
#' an aggregate prediction over all \code{tree} objects held by the
#' \code{forest} will be generated. For \code{"probs"}, a matrix of estimated
#' prediction probabilities will be calculated for each response class and
#' observation in \code{data}.
#' @param ... Additional arguments.
#' @details The prediction probability matrix in fact gives a more detailed
#' picture of class predictions.
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
#' # Make prediction (aggregate/mode)
#' pred <- predict(fit, testData, type = "aggr")
#' # Make prediction (probabilites)
#' pred <- predict(fit, testData, type = "prob")
predict.forest <- function(object, data, type = c("aggr", "prob"), ...) {
  # Check if type matches either "aggr" or "prob"
  type <- match.arg(type)
  b <- length(object$ensemble)
  pred <- list()
  # Get prediction for each tree in ensemble
  for(i in 1:b) {
    pred[[i]] <- predict.tree(object$ensemble[[i]], data)
  }
  # Convert prediction list to data frame
  pred <- data.frame(pred)
  # Remove cryptic colnames
  colnames(pred) <- NULL
  # Get levels from some column of pred to re-factor after using apply()
  lvls <- levels(pred[,1])
  # Write aggregate predictions (using mode), if chosen
  if(type == "aggr") {
    # Apply yields atomic vectors, use lvls to re-factor
    ans <- factor(apply(pred, 1, getMode), levels = lvls)
  } else {
    # Matrix of prediction probabilities for each class and obs.
    ans <- matrix(nrow = nrow(pred), ncol = length(lvls))
    colnames(ans) <- lvls
    for(row in 1:nrow(ans)) {
      # Calc. prediction probs, coerce as vector to fit into matrix
      ans[row,] <- as.vector(table(unlist(pred[row,]))/length(pred[row,]))
    }
  }
  ans
}
