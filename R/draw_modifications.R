#' Draw modification capsules attached to a node.
#'
#' @param x Node center x.
#' @param y Node center y.
#' @param width Node width.
#' @param height Node height.
#' @param mods Character vector of modifications.
#' @param node_id Node identifier for format lookup.
#' @param format_data Format data list.
#' @param detailed_view Whether to show detailed labels (e.g., P@S75).
#'
#' @return NULL. Draws on the active device.
#' @keywords internal
draw_modifications <- function(
    x,
    y,
    width,
    height,
    mods,
    node_id,
    format_data,
    detailed_view = FALSE
  ) {
  mod_padding <- pixels_to_user_x(MOD_CAPSULE_SIDE_PADDING_PX)
  mod_gap <- pixels_to_user_x(MOD_CAPSULE_GAP_PX)

  if (length(mods) == 0) {
    return(invisible(NULL))
  }
  mods <- mods[mods != ""]
  if (length(mods) == 0) {
    return(invisible(NULL))
  }
  mods <- mods[seq_len(min(length(mods), MOD_MAX_PER_ROW))]
  top_y <- y + height / 2
  bottom_y <- y - height / 2

  mod_indices <- seq_along(mods)
  top_mods <- mods[mod_indices %% 2 == 1]
  bottom_mods <- mods[mod_indices %% 2 == 0]

  build_row <- function(row_mods) {
    if (length(row_mods) == 0) {
      return(list(positions = numeric(0), labels = character(0), colors = character(0)))
    }
    labels <- character(length(row_mods))
    colors <- character(length(row_mods))
    widths <- numeric(length(row_mods))
    for (i in seq_along(row_mods)) {
      mod_id <- row_mods[i]
      mod_format <- get_mod_format(node_id, mod_id, format_data)
      labels[i] <- build_mod_label(mod_id, mod_format, detailed_view)
      colors[i] <- if (is.null(mod_format)) {
        format_data$default_node_color
      } else {
        mod_format$color
      }
      widths[i] <- measure_mod_capsule(labels[i])$width
    }
    row_width <- sum(widths) + mod_gap * max(0, length(widths) - 1)
    left_edge <- max(x - width / 2 + mod_padding, x - row_width / 2)
    positions <- numeric(length(widths))
    cursor <- left_edge
    for (i in seq_along(widths)) {
      positions[i] <- cursor + widths[i] / 2
      cursor <- cursor + widths[i] + mod_gap
    }
    list(positions = positions, labels = labels, colors = colors)
  }

  top_row <- build_row(top_mods)
  for (i in seq_along(top_row$positions)) {
    draw_mod_capsule(top_row$positions[i], top_y, top_row$labels[i], top_row$colors[i])
  }

  bottom_row <- build_row(bottom_mods)
  for (i in seq_along(bottom_row$positions)) {
    draw_mod_capsule(bottom_row$positions[i], bottom_y, bottom_row$labels[i], bottom_row$colors[i])
  }
}
