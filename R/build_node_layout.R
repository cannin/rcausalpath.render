#' Build node metadata and layout.
#'
#' @param edges Data.frame returned by read_causalpath_sif.
#' @param format_data Format data list for modification labels.
#' @param layout_method igraph layout function name.
#' @param detailed_view Whether to show detailed modification labels.
#'
#' @return A data.frame with node, x, y, width, height.
#' @keywords internal
build_node_layout <- function(
    edges,
    format_data = NULL,
    layout_method = LAYOUT_METHOD,
    detailed_view = FALSE
  ) {
  nodes <- sort(unique(c(edges$source, edges$target)))
  nodes <- nodes[nodes != ""]
  degree_table <- table(c(edges$source, edges$target))
  nodes <- names(sort(degree_table[nodes], decreasing = TRUE))
  if (is.null(format_data)) {
    format_data <- list(
      default_node_color = DEFAULT_NODE_COLOR,
      node_colors = list(),
      mod_formats = list()
    )
  } else if (is.null(format_data$mod_formats)) {
    format_data$mod_formats <- list()
  }

  if (requireNamespace("igraph", quietly = TRUE)) {
    layout <- build_force_layout(edges, nodes, layout_method)
    layout <- scale_layout(layout)
  } else {
    node_count <- length(nodes)
    angles <- seq(0, 2 * pi, length.out = node_count + 1)[-(node_count + 1)]
    layout <- data.frame(
      node = nodes,
      x = cos(angles),
      y = sin(angles),
      stringsAsFactors = FALSE
    )
    layout <- scale_layout(layout)
  }

  layout$width <- NA_real_
  layout$height <- NA_real_
  mods_by_target <- split(edges$target_mods[edges$target != ""], edges$target[edges$target != ""])
  mod_padding <- pixels_to_user_x(MOD_CAPSULE_SIDE_PADDING_PX)
  mod_gap <- pixels_to_user_x(MOD_CAPSULE_GAP_PX)
  for (i in seq_len(nrow(layout))) {
    label <- layout$node[i]
    text_width <- graphics::strwidth(label, cex = NODE_TEXT_CEX)
    text_height <- graphics::strheight(label, cex = NODE_TEXT_CEX)
    layout$width[i] <- text_width + 2 * NODE_PADDING_X
    layout$height[i] <- (text_height + 2 * NODE_PADDING_Y) * 1.1
    mod_entries <- mods_by_target[[label]]
    if (is.null(mod_entries)) {
      next
    }
    mod_str <- paste(mod_entries, collapse = ";")
    mod_list <- trimws(unlist(strsplit(mod_str, ";", fixed = TRUE)))
    mod_list <- mod_list[mod_list != ""]
    if (length(mod_list) == 0) {
      next
    }
    mod_list <- mod_list[seq_len(min(length(mod_list), MOD_MAX_PER_ROW))]
    mod_indices <- seq_along(mod_list)
    top_mods <- mod_list[mod_indices %% 2 == 1]
    bottom_mods <- mod_list[mod_indices %% 2 == 0]
    row_width <- 0
    if (length(top_mods) > 0) {
      top_widths <- numeric(length(top_mods))
      for (j in seq_along(top_mods)) {
        mod_id <- top_mods[j]
        mod_format <- get_mod_format(label, mod_id, format_data)
        mod_label <- build_mod_label(mod_id, mod_format, detailed_view)
        top_widths[j] <- measure_mod_capsule(mod_label)$width
      }
      row_width <- max(row_width, sum(top_widths) + mod_gap * max(0, length(top_widths) - 1))
    }
    if (length(bottom_mods) > 0) {
      bottom_widths <- numeric(length(bottom_mods))
      for (j in seq_along(bottom_mods)) {
        mod_id <- bottom_mods[j]
        mod_format <- get_mod_format(label, mod_id, format_data)
        mod_label <- build_mod_label(mod_id, mod_format, detailed_view)
        bottom_widths[j] <- measure_mod_capsule(mod_label)$width
      }
      row_width <- max(row_width, sum(bottom_widths) + mod_gap * max(0, length(bottom_widths) - 1))
    }
    if (row_width > 0) {
      layout$width[i] <- max(layout$width[i], row_width + 2 * mod_padding)
    }
  }
  layout
}
