#' Render the causal network on the active device.
#'
#' @param edges Data.frame of edges.
#' @param format_data Format data list for coloring.
#' @param layout_method igraph layout function name.
#' @param detailed_view Whether to show detailed modification labels.
#'
#' @return NULL. Draws to the active device.
#' @keywords internal
render_causalpath <- function(
    edges,
    format_data,
    layout_method = LAYOUT_METHOD,
    detailed_view = FALSE
  ) {
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(CANVAS_X_MIN, CANVAS_X_MAX), ylim = c(CANVAS_Y_MIN, CANVAS_Y_MAX))

  layout <- build_node_layout(edges, format_data, layout_method, detailed_view)
  node_index <- stats::setNames(seq_len(nrow(layout)), layout$node)

  for (i in seq_len(nrow(edges))) {
    src <- edges$source[i]
    tgt <- edges$target[i]
    if (!nzchar(src) || !nzchar(tgt) || !nzchar(edges$edge_type[i])) {
      next
    }
    src_idx <- match(src, layout$node)
    tgt_idx <- match(tgt, layout$node)
    if (is.na(src_idx) || is.na(tgt_idx)) {
      next
    }
    draw_edge(
      layout$x[src_idx], layout$y[src_idx], layout$width[src_idx], layout$height[src_idx],
      layout$x[tgt_idx], layout$y[tgt_idx], layout$width[tgt_idx], layout$height[tgt_idx],
      edges$edge_type[i]
    )
  }

  for (i in seq_len(nrow(layout))) {
    node_id <- layout$node[i]
    node_color <- get_node_color(node_id, format_data)
    draw_node_box(layout$x[i], layout$y[i], layout$width[i], layout$height[i], node_id, node_color)
  }

  mods_by_target <- split(edges$target_mods[edges$target != ""], edges$target[edges$target != ""])
  for (target in names(mods_by_target)) {
    idx <- node_index[[target]]
    mod_str <- paste(mods_by_target[[target]], collapse = ";")
    mod_list <- trimws(unlist(strsplit(mod_str, ";", fixed = TRUE)))
    draw_modifications(
      layout$x[idx],
      layout$y[idx],
      layout$width[idx],
      layout$height[idx],
      mod_list,
      target,
      format_data,
      detailed_view
    )
  }
}
