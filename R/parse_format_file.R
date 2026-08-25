#' Parse a format file to get node and modification colors.
#'
#' @param format_path Path to the format TSV file.
#'
#' @return A list with default_node_color, node_colors, mod_formats.
#' @keywords internal
parse_format_file <- function(format_path) {
  format_data <- list(
    default_node_color = DEFAULT_NODE_COLOR,
    node_colors = list(),
    mod_formats = list()
  )
  if (is.null(format_path) || !nzchar(format_path) || !file.exists(format_path)) {
    return(format_data)
  }
  fmt <- utils::read.delim(
    format_path,
    header = FALSE,
    sep = "\t",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
  )
  if (ncol(fmt) < 4) {
    return(format_data)
  }
  colnames(fmt)[1:4] <- c("entity_type", "entity_id", "format_type", "format")
  fmt$entity_type <- trimws(fmt$entity_type)
  fmt$entity_id <- trimws(fmt$entity_id)
  fmt$format_type <- trimws(fmt$format_type)
  fmt$format <- trimws(fmt$format)

  fmt <- fmt[fmt$entity_type == "node", , drop = FALSE]
  if (nrow(fmt) == 0) {
    return(format_data)
  }

  color_rows <- fmt[fmt$format_type == "color", , drop = FALSE]
  if (nrow(color_rows) > 0) {
    for (i in seq_len(nrow(color_rows))) {
      entity_id <- color_rows$entity_id[i]
      color_value <- parse_rgb_color(color_rows$format[i], DEFAULT_NODE_COLOR)
      if (entity_id == "all-nodes") {
        format_data$default_node_color <- color_value
      } else {
        format_data$node_colors[[entity_id]] <- color_value
      }
    }
  }

  mod_rows <- fmt[fmt$format_type == "modification", , drop = FALSE]
  if (nrow(mod_rows) > 0) {
    for (i in seq_len(nrow(mod_rows))) {
      entity_id <- mod_rows$entity_id[i]
      entries <- unlist(strsplit(mod_rows$format[i], ";", fixed = TRUE))
      entries <- trimws(entries)
      for (entry in entries) {
        if (!nzchar(entry)) {
          next
        }
        parts <- strsplit(entry, "\\|", fixed = FALSE)[[1]]
        if (length(parts) < 3) {
          next
        }
        mod_id <- trimws(parts[1])
        mod_label <- trimws(parts[2])
        mod_color <- parse_rgb_color(parts[3], format_data$default_node_color)
        if (!nzchar(mod_id)) {
          next
        }
        if (is.null(format_data$mod_formats[[entity_id]])) {
          format_data$mod_formats[[entity_id]] <- list()
        }
        format_data$mod_formats[[entity_id]][[mod_id]] <- list(
          color = mod_color,
          label = mod_label
        )
      }
    }
  }
  format_data
}
