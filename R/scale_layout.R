#' Scale a layout into the drawing canvas.
#'
#' @param layout Data.frame with x and y columns.
#' @param margin_ratio Fraction of canvas reserved as margin.
#'
#' @return Layout data.frame with scaled x and y.
#' @keywords internal
scale_layout <- function(layout, margin_ratio = LAYOUT_MARGIN_RATIO) {
  x_range <- range(layout$x)
  y_range <- range(layout$y)
  x_span <- ifelse(diff(x_range) == 0, 1, diff(x_range))
  y_span <- ifelse(diff(y_range) == 0, 1, diff(y_range))

  layout$x <- (layout$x - x_range[1]) / x_span
  layout$y <- (layout$y - y_range[1]) / y_span

  x_margin <- (CANVAS_X_MAX - CANVAS_X_MIN) * margin_ratio
  y_margin <- (CANVAS_Y_MAX - CANVAS_Y_MIN) * margin_ratio
  layout$x <- CANVAS_X_MIN + x_margin + layout$x * ((CANVAS_X_MAX - CANVAS_X_MIN) - 2 * x_margin)
  layout$y <- CANVAS_Y_MIN + y_margin + layout$y * ((CANVAS_Y_MAX - CANVAS_Y_MIN) - 2 * y_margin)
  layout
}
