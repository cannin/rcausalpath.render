#' Read in a binary SIF file.
#'
#' @param input_file Path to the input SIF file.
#'
#' @return A data.frame with the interactions in the binary SIF format.
#'
#' @examples
#' results <- read_sif(system.file("extdata", "test_sif.txt", package = "paxtoolsr"))
#'
#' @concept paxtoolsr
#' @export
read_sif <- function(input_file) {
  results <- utils::read.delim(
    input_file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE
  )
  colnames(results) <- c("PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B")

  results
}
