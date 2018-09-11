
#' Create a categorical variable
#' 
#' @param x variable to which to add th classification
#' @param classification classification to add to \code{x}
#' @param ... when \code{classification} is missing \code{\link{as_categorical}}
#'   is called with these arguments to create the classification
#'   
#' @export
as_categorical <- function(x, ..., classification) {
  if (missing(classification)) {
    classification <- as_classification(...)
  }
  attr(x, "classification") <- classification
  x
}