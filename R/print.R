
#' @export
print.classification <- function(x, short = FALSE, ...) {
  cat("Classification with ", length(x), " levels\n", sep = "")
  for (i in seq_along(x)) {
    if (short) {
      cat("Level ", i, ": ", nrow(x[[i]]), " categories.\n", sep = "")
    } else{
      cat("\n=== Level ", i, ": ", nrow(x[[i]]), " categories.\n", sep = "")
      if (nrow(x[[i]]) > 20) {
        print(utils::head(x[[i]], 10))
        cat("...\n")
      } else {
        print(x[[i]])
      }
    }
  }
}
