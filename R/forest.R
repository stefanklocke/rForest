#' @title Create Classification 'forest' Object
#' @description Creates a fitted classification \code{forest} model. If random
#' subsets of predictors are used for tree construction (via choice of \code{m})
#' , the resulting model is a "Random Forest".
#' @param formula A \code{formula} containing variable names of response and
#' predictors.
#' @param data data.frame of data to fit the model on.
#' @param m Size of predictor subset. If \code{m} is chosen smaller than the
#' number of predictors, a random subset of the specified predictors will be
#' used for tree construction.
#' @param b Number of bootstrap samples from \code{data} and number of trees in
#' forest/ensemble.
#' @param minsplit Minimum number of observations needed for a split.
#' @param minbucket Minimum number of observations that need to be contained
#' each of the left and right groups after a node was split.
#' @param maxdepth Maximum tree depth. The root node has \code{depth = 0}, so
#' that \code{maxdepth = 3} constrains the tree to 4 levels.
#' @param bsinclude Memory intensive! Includes all bootstraps samples in the
#' \code{forest} object, if set \code{TRUE}. Used for test puporses.
#' @details Constructs one classification \code{tree} per bootstrap sample. For
#' a total of \code{b} bootstrap samples, all trees are contained in a tree
#' \code{ensemble} within the generated \code{forest} object.
#' @return An object of class \code{forest}, containing the following elements:
#' An \code{ensemble} holding \code{b} \code{tree} objects. A list of
#' \code{names} for response and predictor names as well as response classes.
#' \code{frequencies} holds \code{data}'s response class frequencies.
#' \code{importance} holds a ranking of predictor importance, measured by the
#' mean information gain relative to the predictor with the highest mean gain.
#' \code{control} holds the options as specified in the function call, as well
#' as \code{random = FALSE} if all predictors were considered for tree
#' construction. Bootstrap samples are stored in \code{bootstraps} if
#' \code{bsinclude = TRUE}.
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
forest <- function(formula,
                   data,
                   m,
                   b = 5,
                   minsplit = 20,
                   minbucket = round(minsplit/3),
                   maxdepth = 30,
                   bsinclude = FALSE) {
  call <- match.call()
  # Extract variable names from formula
  response <- as.character(formula[[2]])
  if(formula[[3]] == ".") {
    predictors <- setdiff(colnames(data), response)
  } else {
    predictors <- all.vars(formula[[3]])
  }
  if(m > length(predictors)) stop(paste("m cannot be greater than number",
                                        " of predictors!", sep = ""))
  # Keep predictors as specified in formula for documentation (in case m < p)
  predictorsCall <- predictors
  # Get (unique) response classes
  if(is.factor(data[[response]])) {
    classes <- levels(data[[response]])
  } else {
    classes <- unique(data[[response]])
  }
  # "Random Forest" if m < length(predictors), else "Bagging"
  random <- FALSE
  if(m < length(predictors)) {
    # 'random' will be stored as a forest property below
    random <- TRUE
  }
  # Initialize lists for tree ensemble and boostrap samples
  ensemble <- list()
  bootstraps <- list()
  # Draw b bootstrap samples and create a tree for each sample
  i <- 1
  while (i <= b) {
    bs <- bootstrap(data)
    # Track bootstrap response class distribution
    if(i == 1) {
      # Initialize on first iteration
      tbl <- table(bs[[response]])
    } else {
      # Add for all i > 1
      tbl <- tbl + table(bs[[response]])
    }
    # Include bootstrap samples if desired: Heavy memory usage!!
    if(bsinclude) {
      bootstraps[[i]] <- bs
    }
    # Create tree object on bootstrap sample
    tree <- tree(formula,
                 data = bs,
                 m,
                 minsplit,
                 minbucket,
                 maxdepth)
    # Add tree to ensemble
    ensemble[[i]] <- tree
    i <- i + 1
  }
  # Relative response class distribution
  tbl <- (tbl/b)/100
  # Assign class ensemble
  class(ensemble) <- "ensemble"
  # Assemble forest object
  ans <- list(ensemble = ensemble,
              names = list(response = response,
                           predictors = predictors,
                           predictorsCall = predictorsCall,
                           classes = classes),
              frequencies = table(data[[response]]),
              frequenciesBsRel = tbl,
              importance = NULL,
              call = call,
              control = list(minsplit = minsplit,
                             minbucket = minbucket,
                             maxdepth = maxdepth,
                             random = random),
              bootstraps = bootstraps)
  class(ans) <- "forest"
  # Generate variable importance ranking (since 'forest' class is assigned now)
  ans$importance <- importance.forest(ans, predictors)
  ans
}
