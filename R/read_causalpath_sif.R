#' Read a CausalPath SIF file.
#'
#' @param sif_path Path to the tab-delimited SIF file.
#'
#' @return A data.frame with columns: source, edge_type, target, data_link, target_mods.
#' @export
read_causalpath_sif <- function(sif_path) {
  sif <- utils::read.delim(
    sif_path,
    header = FALSE,
    sep = "\t",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
  )
  if (ncol(sif) < 3) {
    stop("SIF file must have at least 3 columns: source, edge_type, target.")
  }
  while (ncol(sif) < 5) {
    sif[[ncol(sif) + 1]] <- ""
  }
  colnames(sif)[1:5] <- c("source", "edge_type", "target", "data_link", "target_mods")
  sif$source <- trimws(ifelse(is.na(sif$source), "", sif$source))
  sif$edge_type <- trimws(ifelse(is.na(sif$edge_type), "", sif$edge_type))
  sif$target <- trimws(ifelse(is.na(sif$target), "", sif$target))
  sif$data_link <- trimws(ifelse(is.na(sif$data_link), "", sif$data_link))
  sif$target_mods <- trimws(ifelse(is.na(sif$target_mods), "", sif$target_mods))
  sif
}
