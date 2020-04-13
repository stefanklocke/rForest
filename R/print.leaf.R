#' @title Print Content of a 'leaf' Objects
#' @description Prints response variable's class frequencies from observations
#' inside a \code{node} object, along with the predicted class (mode) and a
#' prediction error rate.
#' @param x Object of class \code{leaf}.
#' @param blank Not passed by user. Used internally for recursion to indent
#' printed text according to a node's/leaf's depth within the sub(tree)
#' structure.
#' @param ... Additional arguments passed to or from other methods..
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
#' # Root's left node is a leaf
#' fit$root$left
#' # Alternatively
#' print(fit$root$left)
print.leaf <- function(x, blank = "", ...) {
  response <- x$names$response
  # If leaf is reached, print predicted class (mode) and prediction error rate
  freq <- table(x$obs[[response]])
  cat(blank,
      "Classes:",
      parenthesize.table(freq, type = "counts"),
      " Prediction: ",
      as.character(x$prediction),
      " (Error: ",
      round(x$predictionErr*100, digits = 2),
      "%)\n", sep = "")
}
