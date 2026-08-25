\name{filter_sif}
\alias{filter_sif}
\title{Filter SIF Interactions}
\description{Filter interactions in a SIF data.frame based on IDs or metadata.}
\usage{filter_sif(sif, ids = NULL, interaction_types = NULL, data_sources = NULL,
  interaction_pubmed_ids = NULL, pathway_names = NULL, mediator_ids = NULL,
  edge_list = NULL, ids_both_participants = FALSE, edge_list_check_reverse = TRUE,
  verbose = FALSE)}
\arguments{
  \item{sif}{A binary SIF data.frame with columns \code{PARTICIPANT_A},
    \code{INTERACTION_TYPE}, and \code{PARTICIPANT_B}.}
  \item{ids}{A vector of IDs to be kept.}
  \item{interaction_types}{A vector of interaction types to be kept.}
  \item{data_sources}{A vector of data sources to be kept. For Extended SIF.}
  \item{interaction_pubmed_ids}{A vector of PubMed IDs to be kept. For Extended SIF.}
  \item{pathway_names}{A vector of pathway names to be kept. For Extended SIF.}
  \item{mediator_ids}{A vector of mediator IDs to be kept. For Extended SIF.}
  \item{edge_list}{A two-column data.frame where each row is an interaction to be kept.}
  \item{ids_both_participants}{Whether both interaction participants should be in \code{ids}.}
  \item{edge_list_check_reverse}{Whether to check for edges in reverse order.}
  \item{verbose}{Whether to emit summary messages.}
}
\value{Filtered interactions with the same class as the input.}
\examples{
\dontrun{
example_sif <- data.frame(
  PARTICIPANT_A = c("A", "B"),
  INTERACTION_TYPE = c("phosphorylates", "phosphorylates"),
  PARTICIPANT_B = c("B", "C"),
  stringsAsFactors = FALSE
)
filter_sif(example_sif, ids = c("A", "B"))
}
}
