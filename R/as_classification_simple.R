


as_classification_simple <- function(id, label) {
  structure(list(
    data.frame(
      id = id,
     label = label
  )), class = "classification")
}
