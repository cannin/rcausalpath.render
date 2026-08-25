#' Get modification formatting for a node.
#'
#' @param node_id Node identifier.
#' @param mod_id Modification identifier.
#' @param format_data Format data list.
#'
#' @return List with color and label or NULL if not found.
#' @keywords internal
get_mod_format <- function(node_id, mod_id, format_data) {
  node_formats <- format_data$mod_formats[[node_id]]
  if (is.null(node_formats)) {
    return(NULL)
  }
  node_formats[[mod_id]]
}
