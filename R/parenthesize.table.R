#' @title Parenthesize
#' @description Pre-processes information of some \code{object} for printing.
#' @param object An object from which information is extracted.
#' @param ... Additional arguments.
#' @details Defined as generic function: \code{parenthesize(object, ...)} can
#' be added as method for other classes.
#' @rdname Parenthesize
#' @export
parenthesize <- function(object, ...) UseMethod("parenthesize")

#' @title Parenthesize Contents of Table Object
#' @description Primarily used for function \code{print.tree()}. Combines
#' table contents (counts or names) to a string, seperated by backslashes and
#' enclosed by parantheses. Provides a handy notation of class frequencies and
#' names to be printed using e.g. \code{cat()}.
#' @param object Object of class \code{table}.
#' @param type Include either class \code{counts} or \code{names}.
#' @param ... Additional arguments.
#' @return Returns a string of a variable's class counts or names. Counts or
#' names are separated by backslashed "\". Note: String contains double "\\",
#' because "\" needs to be escaped as "\\" for printing using \code{cat()}.
#' String is surrounded by parantheses.
#' @method parenthesize table
#' @export
#' @examples
#' tbl <- table(iris$Species)
#' printMe <- parenthesize(tbl, type = "names")
#' cat(printMe)
#' printMe <- parenthesize(tbl, type = "counts")
#' cat(printMe)
parenthesize.table <- function(object, type = c("counts", "names"), ...) {
  # Check if type matches either "counts" or "names"
  type <- match.arg(type)

  if(type == "counts") {
    out <- as.integer(object)
  } else {
    out <- names(object)
  }

  ans <- "("
  for(i in 1:length(out)) {
    # first \ used to escape second \. Only 1 \ will be printed when using cat()
    ans <- paste0(ans, out[i], sep = "\\")
  }
  ans <- paste(substr(ans, 1, nchar(ans)-1), ")", sep = "")
  ans
}
