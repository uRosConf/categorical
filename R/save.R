
#' Save classification to file
#' 
#' @param classification object of type \code{classification}.
#' @param filename the filename of the file to save to. When the extension is
#'   '.csv' or '.CSV' the classification is saved as a CSV file. 
#' @param binary save as a binary (RDS) file. Otherwise save as a CSV file.
#' 
#' @export
save_classification <- function(classification, filename, binary = TRUE) {
  ext <- tools::file_ext(filename)
  if (tolower(ext) == "csv") {
    binary <- FALSE
    warning("Saving as CSV")
  }
  if (tolower(ext) == "rds" && !binary){
    warning("Saving to an RDS file with binary is FALSE. Saving as CSV.")
  }
  if (binary) {
    saveRDS(classification, file = filename)
  } else {
    classification <- do.call(rbind, classification)
    write.csv(classification, filename, row.names = FALSE)
  }
}
