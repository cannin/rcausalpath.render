#' Plot a CausalPath SIF network to PNG and SVG outputs.
#'
#' @param input_sif Path to input SIF file.
#' @param output_png Output PNG file path or NULL to skip PNG.
#' @param output_svg Output SVG file path or NULL to skip SVG.
#' @param format_file Optional format TSV path for coloring.
#' @param layout_method igraph layout function name.
#' @param detailed_view Whether to show detailed modification labels.
#'
#' @return Invisible list with output paths.
#' @export
plot_causalpath <- function(
    input_sif,
    output_png = derive_output_path(input_sif, ".png"),
    output_svg = derive_output_path(input_sif, ".svg"),
    format_file = derive_format_path(input_sif),
    layout_method = LAYOUT_METHOD,
    detailed_view = FALSE
  ) {
  edges <- read_causalpath_sif(input_sif)
  format_data <- parse_format_file(format_file)

  if (!is.null(output_png)) {
    grDevices::png(output_png, width = PNG_WIDTH, height = PNG_HEIGHT, res = 150)
    render_causalpath(edges, format_data, layout_method, detailed_view)
    grDevices::dev.off()
  }

  if (!is.null(output_svg)) {
    grDevices::svg(filename = output_svg, width = SVG_WIDTH_IN, height = SVG_HEIGHT_IN)
    render_causalpath(edges, format_data, layout_method, detailed_view)
    grDevices::dev.off()
  }

  invisible(list(png = output_png, svg = output_svg))
}
