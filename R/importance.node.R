#' @title Predictor Importance
#' @description Generic function for generating a variable importance ranking.
#' @param object Some object.
#' @param ... Additional arguments.
#' @details Defined as generic function: \code{importance(object, ...)} can
#' be added as method for other classes.
#' @rdname Importance
#' @export
importance <- function(object, ...) UseMethod("importance")

#' @title Predictor Importance of Fitted 'tree' Objects
#' @description Generates a ranking of predictor variable importance with
#' respect to their mean information gain relative to the predictor with the
#' highest mean gain.
#' @param object Object of class \code{tree}.
#' @param predictors Set of predictors as specified in the \code{formula}
#' argument of the constructor \code{tree()}.
#' @param ... Additional arguments.
#' @details Ranking of predictor importance is measured by the mean information
#' gain relative to the predictor with the highest mean gain. Note:
#' \code{relMeanGain = NA} means that this predictor was specified in the
#' \code{formula} argument (of the constructor \code{tree()}) but randomly
#' dropped out of consideration due to the choice of \code{m} (random predictor
#' subset).
#' @return data.frame containing ranked variable importance (highest to lowest
#' \code{relMeanGain}).
#' @import stats
#' @export
importance.tree <- function(object, predictors, ...) {
  # Data frame with all split rules
  splits <- traverse.node(object$root)
  # Aggregate split rules (mean) over variables
  aggr <- aggregate(gain ~ var, FUN = mean, data = splits)
  # Order variable importance (decreasing)
  aggr <- aggr[order(aggr$gain, decreasing = TRUE),]
  # Add predictors that randomly dropped out due to p < length(predictors)
  unused <- setdiff(predictors, aggr$var)
  unused <- data.frame(var = unused, gain = rep(NA, times = length(unused)))
  aggr <- rbind(aggr, unused)

  row.names(aggr) <- 1:nrow(aggr)
  colnames(aggr) <- c("var", "relMeanGain")

  # Mean gain relative to variable with highest mean gain
  aggr$relMeanGain <- aggr$relMeanGain / aggr$relMeanGain[1]
  aggr
}
