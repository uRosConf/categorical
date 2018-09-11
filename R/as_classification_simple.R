

#' Create classification with only one level
#'
#' @param id vector of category ids
#' @param label vector of category labels
#'
#' @export
as_classification_simple <- function(id, label) {
  structure(list(
    data.frame(
      id = as.character(id),
     label = as.character(label)
  )), class = "classification")
}
