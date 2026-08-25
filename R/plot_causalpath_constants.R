# Configuration constants.
CANVAS_X_MIN <- -1.6
CANVAS_X_MAX <- 1.6
CANVAS_Y_MIN <- -1.6
CANVAS_Y_MAX <- 1.6
PNG_WIDTH <- 1400
PNG_HEIGHT <- 1400
SVG_WIDTH_IN <- PNG_WIDTH / 150
SVG_HEIGHT_IN <- PNG_HEIGHT / 150
NODE_TEXT_CEX <- 0.7
NODE_PADDING_X <- 0.08
NODE_PADDING_Y <- 0.05
NODE_FILL <- "white"
NODE_BORDER <- "black"
EDGE_WIDTH <- 1.2
EDGE_PADDING <- 0.02
ARROW_LENGTH <- 0.0576
ARROW_ANGLE <- 20
INHIBITION_BAR_LENGTH <- 0.06
MOD_RADIUS <- 0.035
MOD_CAPSULE_PAD_X <- 0.03
MOD_CAPSULE_PAD_Y <- 0.02
MOD_CAPSULE_SIDE_PADDING_PX <- 12
MOD_CAPSULE_GAP_PX <- 6
MOD_TEXT_CEX <- 0.55
MOD_FILL <- "#f0f0f0"
MOD_BORDER <- "#4d4d4d"
MOD_MAX_PER_ROW <- 6
LAYOUT_SEED <- 42
LAYOUT_MARGIN_RATIO <- 0.08
DEFAULT_NODE_COLOR <- "white"
LAYOUT_METHOD <- "layout_with_fr"

EDGE_STYLES <- list(
  phosphorylates = list(color = "#2ca25f", type = "arrow", lty = 1),
  dephosphorylates = list(color = "#de2d26", type = "bar", lty = 1),
  `upregulates-expression` = list(color = "#2ca25f", type = "arrow", lty = 2),
  `downregulates-expression` = list(color = "#de2d26", type = "bar", lty = 2)
)
