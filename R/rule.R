#' @title Find Best Binary Split Rule
#' @description Finds a split rule that yields the highest possible information
#' gain for some \code{data}, \code{response} and a set of \code{predictors}.
#' @param data List or data.frame of data to be split.
#' @param response Response variable from \code{data} over which the information
#' gain will be calculated.
#' @param predictors Vector of predictor variable names to be considered in the
#' search for the best possible split rule.
#' @details Calculates the information gain using \code{gain()} for all values
#' of a predictor over all predictors. The predictor and value with the highest
#' gain are chosen as the best possible binary split rule.
#' @return Returns a list with elements \code{var}, \code{val}, \code{type} and
#' \code{gain}, containing the split rule and the resulting information gain.
#' @export
#' @examples
#' # Two predictors: "Sepal.Width", "Petal.Length"
#' rule(iris, "Species", c("Sepal.Width", "Petal.Length"))
#' # All predictors:
#' rule(iris, "Species", setdiff(colnames(iris), "Species"))
rule <- function(data, response, predictors) {
  # Initialize empty return list and gain as 0
  bestRule <- list()
  bestRule$gain <- 0
  # Calculate initial gini for the first gain to be compared to
  startGini <- gini(data, response)
  # Check all predictor variables
  for(p in 1:length(predictors)) {
    # Current predictor variable name
    splitVar <- predictors[p]
    # Consider each predictor value only once (unique)
    if(is.factor(data[[splitVar]])) {
      values <- levels(unique(data[[splitVar]]))
    } else {
      values <- unique(data[[splitVar]])
    }
    # Check all (unique) predictor values
    for(v in 1:length(values)) {
      splitVal <- values[v]
      # Split data to check for the split's information gain
      currentSplit <- setSplit(data, splitVar = splitVar, splitVal = splitVal)
      # Calculate gain only if current split actually partitions data
      if(nrow(currentSplit$left) == 0 || nrow(currentSplit$right) == 0) {
        # Proceed to next value if split leaves one group empty ("NULL-split")
        next
      }
      # Store gain of current split
      currentGain <- gain(currentSplit$left, currentSplit$right, response,
                          startGini)
      # Check if current split yields higher node purity (higher gain)
      if(currentGain > bestRule$gain) {
        # splitType indicates whether split was numeric or categorial
        if(is.numeric(data[[splitVar]])) {
          splitType <- "numeric"
        } else {
          splitType <- "categorial"
        }
        # Store split characteristics in list
        bestRule <- list(var = splitVar,
                         val = splitVal,
                         type = splitType,
                         gain = currentGain)
      }
    }
  }
  bestRule
}
