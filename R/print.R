
print.level_meta <- function(x, ...) {
  cat("Classification with ", length(x), " levels\n", sep = "")
  for (i in seq_along(x)) {
    cat("\n=== Level ", i, ": ", nrow(x[[i]]), " categories.\n", sep = "")
    if (nrow(x[[i]]) > 20) {
      print(head(x[[i]], 10))
      cat("...\n")
    } else {
      print(x[[i]])
    }
  }
}