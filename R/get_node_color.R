#' Get the node color with fallback to default.
#'
#' @param node_id Node identifier.
#' @param format_data Format data list from parse_format_file.
#'
#' @return R color string.
#' @keywords internal
get_node_color <- function(node_id, format_data) {
  color <- format_data$node_colors[[node_id]]
  if (is.null(color)) {
    return(format_data$default_node_color)
  }
  color
}
