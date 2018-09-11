
#' Complete classification in case branches in tree are not all of same depth
#' 
#' @param x the data.frame to convert. See, details for the format of the
#'   data.frame
#' @param order selection of columns from x.
#' 
#' @details 
#' Some classification do not all go to the same depth for each of the branches. 
#' The function \code{\link{as_classification}} does not support that. This 
#' function repeats categories so that all branches are of the same depth.
#' 
#' @export
complete_classification <- function(x, order = 1:4) {
 # Put input data in right order
  meta = x[order]
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
  
  for(i in 1:(length(df)-1)) {
    for(x in 1:length(df[[i]]$id)) {
      if (is.na(match(df[[i]]$id[x], unique(df[[i+1]]$parent)))) {
        df[[i+1]][nrow(df[[i+1]])+1,] <- 
          c(df[[i]]$id[x],df[[i]]$label[x],i+1,df[[i]]$id[x])
      }
    }
  }
  df
}
