#' Build the display label for a modification site.
#'
#' @param mod_id Modification identifier.
#' @param mod_format Optional format list for the modification.
#' @param detailed_view Whether to show detailed labels.
#'
#' @return Label string to render.
#' @keywords internal
build_mod_label <- function(mod_id, mod_format, detailed_view = FALSE) {
  if (is.null(mod_format)) {
    label <- if (grepl("^[STY]", mod_id)) "P" else ""
  } else if (nzchar(mod_format$label)) {
    label <- mod_format$label
  } else {
    label <- ""
  }
  if (detailed_view) {
    return(if (nzchar(label)) paste0(label, "@", mod_id) else mod_id)
  }
  label
}
