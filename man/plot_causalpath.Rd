\name{plot_causalpath}
\alias{plot_causalpath}
\title{Plot a CausalPath Network}
\description{Plot a CausalPath SIF network to PNG and SVG outputs.}
\usage{plot_causalpath(input_sif, output_png = derive_output_path(input_sif, ".png"),
  output_svg = derive_output_path(input_sif, ".svg"),
  format_file = derive_format_path(input_sif), layout_method = LAYOUT_METHOD,
  detailed_view = FALSE)}
\arguments{
  \item{input_sif}{Path to input SIF file.}
  \item{output_png}{Output PNG file path or NULL to skip PNG.}
  \item{output_svg}{Output SVG file path or NULL to skip SVG.}
  \item{format_file}{Optional format TSV path for coloring.}
  \item{layout_method}{igraph layout function name.}
  \item{detailed_view}{Whether to show detailed modification labels.}
}
\value{Invisible list with output paths.}
\examples{
\dontrun{
sif_path <- system.file(
  "extdata",
  "causalpath_causative_small.sif",
  package = "rcausalpath"
)
format_path <- tempfile(fileext = "_format.tsv")
write_format_file(sif_path, format_path)
plot_causalpath(sif_path, format_file = format_path)
}
}
