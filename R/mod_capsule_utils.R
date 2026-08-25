#' Measure capsule dimensions for a modification label.
#'
#' @param label Text label for the capsule.
#'
#' @return List with width and height in user units.
#' @keywords internal
measure_mod_capsule <- function(label) {
  text_height <- graphics::strheight("X", cex = MOD_TEXT_CEX)
  if (nzchar(label)) {
    text_height <- max(text_height, graphics::strheight(label, cex = MOD_TEXT_CEX))
  }
  capsule_height <- max(2 * MOD_RADIUS, text_height + 2 * MOD_CAPSULE_PAD_Y)
  text_width <- if (nzchar(label)) graphics::strwidth(label, cex = MOD_TEXT_CEX) else 0
  capsule_width <- max(2 * (capsule_height / 2), text_width + 2 * MOD_CAPSULE_PAD_X)
  list(width = capsule_width, height = capsule_height)
}
