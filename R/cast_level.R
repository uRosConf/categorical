


#' @export
#' 
cast_level <- function(x, meta = attr(x, "level_meta"), level = length(meta), 
    label = TRUE) {
  
  # Go recursively up the tree each time adding the labels of the next level
  cur <- x
  if (level < length(meta)) {
    for (i in seq(length(meta), level + 1,-1)) {
      m <- match(cur, meta[[i]]$id)
      if (any(is.na(m))) warning("Missing values in id.")
      cur <- meta[[i]]$parent[m]
    }
  }
  
  # Recode to labels
  if (label) {
    m <- match(cur, meta[[level]]$id)
    if (any(is.na(m))) warning("Missing values in labels.")
    factor(meta[[level]]$label[m], 
      levels = na.omit(unique(meta[[level]]$label)))
  } else {
    factor(cur, 
      levels = na.omit(unique(meta[[level]]$id)))
  }
}

