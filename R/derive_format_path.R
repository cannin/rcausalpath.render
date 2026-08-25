#' Derive a format file path from the input path.
#'
#' @param input_path Path to the input file.
#'
#' @return Path for the format TSV file.
#' @keywords internal
derive_format_path <- function(input_path) {
  base <- sub("\\.[^.]*$", "", input_path)
  if (identical(base, input_path)) {
    paste0(input_path, "_format.tsv")
  } else {
    paste0(base, "_format.tsv")
  }
}
