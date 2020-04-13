#' @title Create Classification 'tree' Object
#' @description Creates a fitted classification \code{tree} model.
#' @param formula A \code{formula} containing variable names of response and
#' predictors.
#' @param data data.frame of data to fit the model on.
#' @param m Size of predictor subset. If \code{m} is chosen smaller than the
#' number of predictors, a random subset of the specified predictors will be
#' used for tree construction.
#' @param minsplit Minimum number of observations needed for a split.
#' @param minbucket Minimum number of observations that need to be contained
#' each of the left and right groups after a node was split.
#' @param maxdepth Maximum tree depth. The root node has \code{depth = 0}, so
#' that \code{maxdepth = 3} constrains the tree to 4 levels.
#' @details Note on variable \code{importance}: \code{meanGain = 0} means that
#' this predictor was taken into account by the tree building process but was
#' not used, since it did not yield information gain.\code{meanGain = NA} means
#' that this predictor was specified in \code{formula} but randomly dropped out
#' of consideration due to the choice of \code{m} (random predictor subset).
#' @return An object of class \code{tree}, containing the following elements:
#' A \code{root} node, which holds the tree structure. A list of \code{names}
#' for response and predictor names as well as response classes (NOTE: The
#' element \code{predictors} holds the names of ALL predictors, even if some
#' were in fact not used as specified by parameter \code{m}). \code{frequencies}
#' holds the root's response class frequencies. \code{importance} holds a
#' ranking of predictor importance, measured by the mean information gain.
#' \code{control} holds the options as specified in the function call, as well
#' as \code{random = FALSE} if all predictors were considered for tree
#' construction.)
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
tree <- function(formula,
                 data,
                 m,
                 minsplit = 20,
                 minbucket = round(minsplit/3),
                 maxdepth = 30) {
  if(nrow(data) < minsplit) stop("Not enough observations to build tree!")
  if(minsplit < 2*minbucket) stop("minsplit needs to be >= 2*minbucket!")
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
    # 'random' will be stored as a tree property below
    random <- TRUE
    # Draw random subset of predictors
    predictors <- sample(predictors, m, replace = FALSE)
  }

  # Give grow() access to tree()'s arguments: Define grow() inside here!
  grow <- function(group, depth = 0) {
    if(nrow(group) < minsplit) {
      setLeaf(group, response, depth)
    }
    # Get best binary split rule
    rule <- rule(group, response, predictors)
    # Do not split, if split does not yield info. gain or maxdepth reached
    if(rule$gain == 0 || depth > maxdepth) {
      setLeaf(group, response, depth)
    } else {
      split <- setSplit(group, rule$var, rule$val)
      # Do not split, if not too few obs. or if resulting leafs/nodes too small
      if(nrow(split$left) < minbucket || nrow(split$right) < minbucket) {
        setLeaf(group, response, depth)
      } else {
        # Recursively process left/right groups resulting from previous split
        left <- grow(split$left, depth = depth + 1)
        right <- grow(split$right, depth = depth + 1)
        # Combine subtrees into node
        setNode(rule, left, right, response, depth = depth)
      }
    }
  }
  # Grow tree
  tree <- grow(data)
  # Assemble tree object
  ans <- list(root = tree,
              names = list(response = response,
                           predictors = predictors,
                           predictorsCall = predictorsCall,
                           classes = classes),
              frequencies = table(data[[response]]),
              importance = NULL,
              call = call,
              control = list(minsplit = minsplit,
                             minbucket = minbucket,
                             maxdepth = maxdepth,
                             random = random))
  class(ans) <- "tree"
  # Generate variable importance ranking (since 'tree' class is assigned now)
  ans$importance <- importance.tree(ans, predictors)
  ans
}
