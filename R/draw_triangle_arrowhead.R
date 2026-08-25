#' Draw a filled triangular arrowhead.
#'
#' @param x Tip x coordinate.
#' @param y Tip y coordinate.
#' @param ux Unit direction x component (pointing to the tip).
#' @param uy Unit direction y component (pointing to the tip).
#' @param color Fill/border color for the triangle.
#'
#' @return NULL. Draws on the active device.
#' @keywords internal
draw_triangle_arrowhead <- function(x, y, ux, uy, color) {
  base_x <- x - ux * ARROW_LENGTH
  base_y <- y - uy * ARROW_LENGTH
  perp_x <- -uy
  perp_y <- ux
  half_width <- ARROW_LENGTH * tan(ARROW_ANGLE * pi / 180)
  x1 <- base_x + perp_x * half_width
  y1 <- base_y + perp_y * half_width
  x2 <- base_x - perp_x * half_width
  y2 <- base_y - perp_y * half_width
  graphics::polygon(c(x, x1, x2), c(y, y1, y2), col = color, border = color)
}
