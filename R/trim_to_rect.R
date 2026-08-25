#' Compute intersection of a line from center with a rectangle boundary.
#'
#' @param x0 Rectangle center x.
#' @param y0 Rectangle center y.
#' @param width Rectangle width.
#' @param height Rectangle height.
#' @param x1 Target point x.
#' @param y1 Target point y.
#'
#' @return Numeric vector c(x, y) for the intersection point.
#' @keywords internal
trim_to_rect <- function(x0, y0, width, height, x1, y1) {
  dx <- x1 - x0
  dy <- y1 - y0
  if (dx == 0 && dy == 0) {
    return(c(x0, y0))
  }
  tx <- if (dx != 0) (width / 2) / abs(dx) else Inf
  ty <- if (dy != 0) (height / 2) / abs(dy) else Inf
  t <- min(tx, ty)
  c(x0 + dx * t, y0 + dy * t)
}
