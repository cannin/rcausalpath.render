#' Draw a modification capsule attached to a node.
#'
#' @param x Center x.
#' @param y Center y.
#' @param label Text label for the modification.
#' @param fill_color Fill color for the capsule.
#'
#' @return NULL. Draws on the active device.
#' @keywords internal
draw_mod_capsule <- function(x, y, label, fill_color = MOD_FILL) {
  capsule_dims <- measure_mod_capsule(label)
  capsule_width <- capsule_dims$width
  radius <- capsule_dims$height / 2
  half_rect <- max(0, capsule_width / 2 - radius)

  left_center <- x - half_rect
  right_center <- x + half_rect
  arc_points <- 30
  left_theta <- seq(pi / 2, 3 * pi / 2, length.out = arc_points)
  right_theta <- seq(3 * pi / 2, 5 * pi / 2, length.out = arc_points)
  x_points <- c(
    left_center + radius * cos(left_theta),
    right_center + radius * cos(right_theta)
  )
  y_points <- c(
    y + radius * sin(left_theta),
    y + radius * sin(right_theta)
  )
  graphics::polygon(
    x_points,
    y_points,
    col = fill_color,
    border = MOD_BORDER
  )

  if (nzchar(label)) {
    graphics::text(x, y, label, cex = MOD_TEXT_CEX)
  }
}
