#' Convert pixel length to user units along the x-axis.
#'
#' @param pixels Pixel length to convert.
#'
#' @return Numeric length in user coordinates.
#' @keywords internal
pixels_to_user_x <- function(pixels) {
  if (!is.finite(pixels) || pixels <= 0) {
    return(0)
  }
  usr <- graphics::par("usr")
  dev_px <- grDevices::dev.size("px")
  if (length(dev_px) < 2 || !is.finite(dev_px[1]) || dev_px[1] <= 0) {
    return(0)
  }
  pixels * (usr[2] - usr[1]) / dev_px[1]
}
