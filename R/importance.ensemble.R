#' @title Predictor Importance of Fitted 'forest' Objects
#' @description \code{forest}-version of \code{importance.tree()}. Generates a
#' ranking of predictor variable importance with respect to their mean
#' information gain relative to the predictor with the highest mean gain.
#' @param object Object of class \code{forest}.
#' @param predictors Set of predictors as specified in the \code{formula}
#' argument of the constructor \code{forest()}.
#' @param ... Additional arguments.
#' @details Ranking of predictor importance is measured by the mean information
#' gain relative to the predictor with the highest mean gain. Note:
#' \code{formula} argument (of the constructor \code{tree()}) but randomly
#' dropped out of consideration due to the choice of \code{m} (random predictor
#' subset).
#' @return data.frame containing ranked variable importance (highest to lowest
#' \code{relMeanGain}).
#' @import stats
#' @export
importance.forest <- function(object, predictors, ...) {
  b <- length(object$ensemble)
  gather <- list()
  for(i in 1:b) {
    gather <- rbind(gather, object$ensemble[[i]]$importance)
  }
  aggr <- aggregate(relMeanGain ~ var, FUN = mean, data = gather)
  aggr <- aggr[order(aggr$relMeanGain, decreasing = TRUE),]

  unused <- setdiff(predictors, aggr$var)
  unused <- data.frame(var = unused, relMeanGain = rep(NA, times = length(unused)))

  aggr <- rbind(aggr, unused)

  row.names(aggr) <- 1:nrow(aggr)

  # Mean gain relative to variable with highest mean gain
  aggr$relMeanGain <- aggr$relMeanGain / aggr$relMeanGain[1]
  aggr
}
