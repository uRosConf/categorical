

#' Convert classification ID to factor a specified level of classification
#' 
#' @param x the vector to cast
#' @param meta an oject of type \code{classification}.
#' @param level the level of the classificatio to which the variable should be
#'   cast. 
#' @param label return the labels of the categories. Otherwise return the 
#'   codes. 
#' 
#' @export
#' 
cast_level <- function(x, meta = attr(x, "classification"), level = length(meta), 
    label = TRUE) {
  
  # Go recursively up the tree each time adding the labels of the next level
  cur <- x
  if (level < length(meta)) {
    for (i in seq(length(meta), level + 1,-1)) {
      m <- match(cur, meta[[i]]$id)
      if (any(is.na(m) & !is.na(cur))) {
        keys <- unique(cur[is.na(m) & !is.na(cur)])
        example <- paste0("'", utils::head(keys, 10), "'", collapse = ", ")
        if (length(keys) > 10) example <- paste0(example, " ...")
        warning(paste0("Invalid classification key in x: ", example))
      }
      cur <- meta[[i]]$parent[m]
    }
  }
  
  # Recode to labels
  if (label) {
    m <- match(cur, meta[[level]]$id)
    if (any(is.na(m))) warning("Invalid classification key in x.")
    factor(meta[[level]]$label[m], 
      levels = stats::na.omit(unique(meta[[level]]$label)))
  } else {
    factor(cur, 
      levels = stats::na.omit(unique(meta[[level]]$id)))
  }
}

