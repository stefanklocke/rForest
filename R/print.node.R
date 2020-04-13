#' @title Print (Sub)Tree structure of 'node' and 'leaf' Objects
#' @description Prints the (sub)tree structure of \code{node} and \code{leaf}
#' objects, including split variables, split values, predictions and class
#' frequencies.
#' @param x Object of class \code{node}.
#' @param blank Not passed by user. Used internally for recursion to indent
#' printed text according to a node's/leaf's depth within the sub(tree)
#' structure.
#' @param ... Additional arguments passed to or from other methods.
#' @details Internally used in \code{print.tree()} for printing the tree
#' structure of a \code{tree} object. While \code{print.tree()} always prints
#' the entire tree, \code{print.node()} can be used to also print subtrees.
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
#' # Print right subtree of root
#' #fit$root$right
#' # Alternatively
#' print(fit$root$right)
print.node <- function(x, blank = "", ...) {
  response <- x$names$response
  # If leaf is reached, print predicted class (mode) and prediction error rate
  if(class(x) == "leaf") {
    print.leaf(x, blank)
  } else {
    # Store relation sign specific to split type numeric/categorial
    if(x$rule$type == "numeric") {
      relation <- c(left = " < ", right = " >= ")
    } else {
      relation <- c(left = " != ", right = " == ")
    }
    # Print a left node's split rule
    cat(blank,
        "--> Left: ",
        x$rule$var,
        relation[["left"]],
        x$rule$val,
        "\n", sep = "")
    # Recurse to left node
    print.node(x$left, paste(blank, "   "))
    # Print a right node's split rule
    cat(blank,
        "--> Right: ",
        x$rule$var,
        relation[["right"]],
        x$rule$val,
        "\n", sep = "")
    # Recurse to right node
    print.node(x$right, paste(blank, "   "))
  }
}
