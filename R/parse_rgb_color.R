#' Parse an RGB string into an R color.
#'
#' @param rgb_text String with space-delimited RGB values.
#' @param default_color Color to return if parsing fails.
#'
#' @return R color string.
#' @keywords internal
parse_rgb_color <- function(rgb_text, default_color = DEFAULT_NODE_COLOR) {
  if (!nzchar(rgb_text)) {
    return(default_color)
  }
  parts <- unlist(strsplit(trimws(rgb_text), "\\s+"))
  if (length(parts) < 3) {
    return(default_color)
  }
  nums <- suppressWarnings(as.numeric(parts[1:3]))
  if (any(is.na(nums))) {
    return(default_color)
  }
  grDevices::rgb(nums[1], nums[2], nums[3], maxColorValue = 255)
}
