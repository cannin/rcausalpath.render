\name{read_sif}
\alias{read_sif}
\title{Read a Binary SIF File}
\description{Read a binary SIF file with three columns.}
\usage{read_sif(input_file)}
\arguments{
  \item{input_file}{Path to the input SIF file.}
}
\value{A data.frame with columns \code{PARTICIPANT_A}, \code{INTERACTION_TYPE}, and \code{PARTICIPANT_B}.}
\examples{
\dontrun{
results <- read_sif("path/to/sif_with_header.txt")
}
}
