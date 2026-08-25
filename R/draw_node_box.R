#' Draw a labeled node box.
#'
#' @param x Center x.
#' @param y Center y.
#' @param width Box width.
#' @param height Box height.
#' @param label Node label.
#' @param fill_color Fill color for the node box.
#'
#' @return NULL. Draws on the active device.
#' @keywords internal
draw_node_box <- function(x, y, width, height, label, fill_color = NODE_FILL) {
  graphics::rect(
    x - width / 2,
    y - height / 2,
    x + width / 2,
    y + height / 2,
    col = fill_color,
    border = NODE_BORDER
  )
  graphics::text(x, y, label, cex = NODE_TEXT_CEX)
}
