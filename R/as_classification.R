

#' Convert a data.frame to a classification object
#' 
#' @param x the data.frame to convert. See, details for the format of the
#'   data.frame
#' @param compute_parent compute the parent column of the input
#' @param order selection of columns from x.
#' 
#' @details 
#' The data.frame should contain the following columns:
#' \describe{
#'   \item{id} {the id of the category (cast to character)}
#'   \item{label} {the label of the category (cast to character)}
#'   \item{level} {the level of the classification in which the category belongs
#'     (should be integer)}
#'   \item{parent} {the id of the parent category (cast to character). Can be
#'     omitted when \code{compute_parent = FALSE}. Should contain missing values
#'     for categories in level 1 of the classification.}
#' }
#'     
#' @export
as_classification <- function(x, compute_parent = FALSE, order = 1:4) {
  # Put input data in right order
  if (compute_parent) order <- order[1:3]
  meta = x[order]
  if (compute_parent) meta[[4]] <- character(nrow(meta))
  # Rename columns
  names(meta) <- c("id", "label", "level", "parent")
  # Check types of meta
  for (col in c(1,2,4)) meta[[col]] <- as.character(meta[[col]])
  stopifnot(is.integer(meta$level))
  if (!all(unique(meta$level) == seq(1, max(meta$level))))
    stop("The levels should be numbered from 1 sequentially up.")
  # Check duplicated ids
  if (any(duplicated(meta$id)))
    stop(paste0("Duplicated id in dataframe. Example:", 
      sample(meta$id[duplicated(meta$id)],1)))
  # Check if tree complete; all same depth
  for (i in seq(1,max(meta$level) - 1)) {
    vals <- meta$id[meta$level == i]
    result <- any(vals %in% meta$parent) == FALSE
    if (result) {
      stop(paste("Level",i,"not in parent column"))
    }
  }
  # Computing parent
  if (compute_parent) {
    for (i in seq(1,max(meta$level))) {
      for (z in meta[meta$level == i,"id"]) {
        meta$parent[meta[,"level"] == i + 1 & 
            substr(meta$id,1,unique(nchar(meta[meta$level == i,"id"]))) == z] <- z
      }
    }
  }
  # Creating meta
  meta <- split(meta, meta$level)
  structure(meta, class = "classification")
}

