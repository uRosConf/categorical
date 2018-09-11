
#' Read classification from file. 
#' 
#' @param filename the file to read from. 
#' @param binary is \code{TRUE} interpret the file as a binary (RDS) file. 
#'   Otherwise the file is interpreted as a CSV file. 
#'   
#' @export
read_classification <- function(filename, binary = TRUE) {
  ext <- tools::file_ext(filename)
  if (tolower(ext) == "csv") {
    binary <- FALSE
    warning("Reading from CSV")
  }
  if (tolower(ext) == "rds" && !binary){
    warning("Reading from file with RDS exension with binary is FALSE. Reading as CSV.")
  }
  if (binary) {
    readRDS(filename)
  } else {
    classification <- read.csv(filename, stringsAsFactors = FALSE)
    as_classification(classification)
  }
}
