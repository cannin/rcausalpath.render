\name{read_causalpath_sif}
\alias{read_causalpath_sif}
\title{Read a CausalPath SIF File}
\description{Read a CausalPath SIF file and return parsed columns.}
\usage{read_causalpath_sif(sif_path)}
\arguments{
  \item{sif_path}{Path to the tab-delimited SIF file.}
}
\value{A data.frame with columns \code{source}, \code{edge_type}, \code{target},
  \code{data_link}, and \code{target_mods}.}
\examples{
\dontrun{
sif_path <- system.file(
  "extdata",
  "causalpath_causative_small.sif",
  package = "rcausalpath"
)
edges <- read_causalpath_sif(sif_path)
}
}
