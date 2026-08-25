\name{write_format_file}
\alias{write_format_file}
\title{Write a Format TSV File}
\description{Generate a format TSV file with random colors for nodes and modifications.}
\usage{write_format_file(input_sif, output_format, seed = 42,
  value_min = -2, value_max = 2)}
\arguments{
  \item{input_sif}{Path to the input SIF file.}
  \item{output_format}{Output format TSV path.}
  \item{seed}{Random seed for reproducibility.}
  \item{value_min}{Minimum random value.}
  \item{value_max}{Maximum random value.}
}
\value{NULL. Writes the format file to disk.}
\examples{
\dontrun{
sif_path <- system.file(
  "extdata",
  "causalpath_causative_small.sif",
  package = "rcausalpath"
)
format_path <- tempfile(fileext = "_format.tsv")
write_format_file(sif_path, format_path)
}
}
