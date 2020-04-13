#' @title Bootstrap Resampling
#' @description Draw a random sample with replacement from some \code{data}. Can
#' be used to resample training data sets in order to construct multiple
#' decision trees (with different tree structure) given a single training set.
#' @param data List or data.frame of data to be resampled.
#' @export
#' @examples bootstrap(iris)
#' bootstrap(iris)
bootstrap <- function(data) {
  rows <- sample(1:nrow(data), replace = TRUE)
  bs <- data[rows,]
  bs
}
