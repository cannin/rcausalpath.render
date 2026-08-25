#' Derive output file names from the input path.
#'
#' @param input_path Path to the input file.
#' @param new_ext Extension to use (including dot).
#'
#' @return File path with the extension replaced.
#' @keywords internal
derive_output_path <- function(input_path, new_ext) {
  base <- sub("\\.[^.]*$", "", input_path)
  if (identical(base, input_path)) {
    paste0(input_path, new_ext)
  } else {
    paste0(base, new_ext)
  }
}
