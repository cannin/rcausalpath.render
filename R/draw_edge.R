#' Draw a single edge with styling based on edge type.
#'
#' @param x0 Source center x.
#' @param y0 Source center y.
#' @param w0 Source width.
#' @param h0 Source height.
#' @param x1 Target center x.
#' @param y1 Target center y.
#' @param w1 Target width.
#' @param h1 Target height.
#' @param edge_type Edge type string.
#'
#' @return NULL. Draws on the active device.
#' @keywords internal
draw_edge <- function(x0, y0, w0, h0, x1, y1, w1, h1, edge_type) {
  style <- EDGE_STYLES[[edge_type]]
  if (is.null(style)) {
    style <- list(color = "#636363", type = "arrow", lty = 1)
  }
  start <- trim_to_rect(x0, y0, w0, h0, x1, y1)
  end <- trim_to_rect(x1, y1, w1, h1, x0, y0)
  dx <- end[1] - start[1]
  dy <- end[2] - start[2]
  dist <- sqrt(dx^2 + dy^2)
  if (dist == 0) {
    return(invisible(NULL))
  }
  ux <- dx / dist
  uy <- dy / dist
  start <- c(start[1] + ux * EDGE_PADDING, start[2] + uy * EDGE_PADDING)
  end <- c(end[1] - ux * EDGE_PADDING, end[2] - uy * EDGE_PADDING)
  graphics::segments(
    start[1],
    start[2],
    end[1],
    end[2],
    col = style$color,
    lwd = EDGE_WIDTH,
    lty = style$lty
  )

  if (style$type == "arrow") {
    draw_triangle_arrowhead(end[1], end[2], ux, uy, style$color)
  } else if (style$type == "bar") {
    bar_half <- INHIBITION_BAR_LENGTH / 2
    perp_x <- -uy
    perp_y <- ux
    bar_x1 <- end[1] - perp_x * bar_half
    bar_y1 <- end[2] - perp_y * bar_half
    bar_x2 <- end[1] + perp_x * bar_half
    bar_y2 <- end[2] + perp_y * bar_half
    graphics::segments(bar_x1, bar_y1, bar_x2, bar_y2, col = style$color, lwd = EDGE_WIDTH)
  }
}
