#' Keep interactions in SIF network based on certain criteria.
#'
#' @param sif A binary SIF as a data.frame with three columns:
#'   "PARTICIPANT_A", "INTERACTION_TYPE", "PARTICIPANT_B".
#' @param ids A vector of IDs to be kept.
#' @param interaction_types A vector of interaction types to be kept
#'   (list: http://www.pathwaycommons.org/pc2/formats).
#' @param data_sources A vector of data sources to be kept. For Extended SIF.
#' @param interaction_pubmed_ids A vector of PubMed IDs to be kept. For Extended SIF.
#' @param pathway_names A vector of pathway names to be kept. For Extended SIF.
#' @param mediator_ids A vector of mediator IDs to be kept. For Extended SIF.
#'   Mediator IDs are the full BioPAX objects that were simplified to interactions.
#' @param edge_list A two-column data.frame where each row is an interaction to be kept.
#'   Directionality is ignored (e.g., edge A B matches A B and B A in the SIF).
#' @param ids_both_participants Whether both interaction participants should be in ids.
#' @param edge_list_check_reverse Whether to check for edges in reverse order.
#' @param verbose Whether to emit summary messages.
#'
#' @return Filtered interactions with three columns: "PARTICIPANT_A",
#'   "INTERACTION_TYPE", "PARTICIPANT_B". The intersection of multiple filters is
#'   returned. The return class matches the input: data.frame or data.table.
#'
#' @examples
#' results <- read_sif(system.file("extdata", "test_sif.txt", package = "paxtoolsr"))
#' int_types <- c("controls-state-change-of", "controls-expression-of", "catalysis-precedes")
#' filtered_network <- filter_sif(results, interaction_types = int_types)
#'
#' tmp <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package = "paxtoolsr"))
#' results <- filter_sif(tmp$edges, ids = c("CHEBI:17640", "MCM3"))
#' results <- filter_sif(tmp$edges, data_sources = c("INOH", "KEGG"))
#' results <- filter_sif(tmp$edges, data_sources = c("IntAct"), ids = c("CHEBI:17640", "MCM3"))
#' results <- filter_sif(tmp$edges, pathway_names = c("Metabolic pathways"))
#' results <- filter_sif(
#'   tmp$edges,
#'   mediator_ids = c("http://purl.org/pc2/8/MolecularInteraction_1452626895158")
#' )
#' results <- filter_sif(tmp$edges, interaction_pubmed_ids = "17654400")
#'
#' tmp <- readSifnx(system.file("extdata", "test_sifnx_250.txt", package = "paxtoolsr"))
#' edge_list <- read.table(
#'   system.file("extdata", "test_edgelist.txt", package = "paxtoolsr"),
#'   sep = "\t",
#'   header = FALSE,
#'   stringsAsFactors = FALSE
#' )
#' results <- filter_sif(tmp$edges, edge_list = edge_list)
#'
#' @concept paxtoolsr
#' @export
filter_sif <- function(
    sif,
    ids = NULL,
    interaction_types = NULL,
    data_sources = NULL,
    interaction_pubmed_ids = NULL,
    pathway_names = NULL,
    mediator_ids = NULL,
    edge_list = NULL,
    ids_both_participants = FALSE,
    edge_list_check_reverse = TRUE,
    verbose = FALSE
  ) {
  idx_list <- list()

  if (!is.null(ids)) {
    a_idx <- which(sif$PARTICIPANT_A %in% ids)
    b_idx <- which(sif$PARTICIPANT_B %in% ids)
    if (isTRUE(ids_both_participants)) {
      idx_ids <- intersect(a_idx, b_idx)
    } else {
      idx_ids <- unique(c(a_idx, b_idx))
    }
    idx_list[["ids"]] <- idx_ids
  }

  if (!is.null(interaction_types)) {
    idx_interaction_types <- which(sif$INTERACTION_TYPE %in% interaction_types)
    idx_list[["interaction_types"]] <- idx_interaction_types
  }

  if (!is.null(data_sources)) {
    if (!"INTERACTION_DATA_SOURCE" %in% colnames(sif)) {
      stop("SIF is missing INTERACTION_DATA_SOURCE for data_sources filtering.")
    }
    results <- search_list_of_vectors(data_sources, sif$INTERACTION_DATA_SOURCE)
    idx_data_sources <- unique(unlist(results))
    idx_list[["data_sources"]] <- idx_data_sources
  }

  if (!is.null(interaction_pubmed_ids)) {
    if (!"INTERACTION_PUBMED_ID" %in% colnames(sif)) {
      stop("SIF is missing INTERACTION_PUBMED_ID for interaction_pubmed_ids filtering.")
    }
    idx_interaction_pubmed_ids <- which(
      sif$INTERACTION_PUBMED_ID %in% interaction_pubmed_ids
    )
    idx_list[["interaction_pubmed_ids"]] <- idx_interaction_pubmed_ids
  }

  if (!is.null(pathway_names)) {
    if (!"PATHWAY_NAMES" %in% colnames(sif)) {
      stop("SIF is missing PATHWAY_NAMES for pathway_names filtering.")
    }
    idx_pathway_names <- which(sif$PATHWAY_NAMES %in% pathway_names)
    idx_list[["pathway_names"]] <- idx_pathway_names
  }

  if (!is.null(mediator_ids)) {
    if (!"MEDIATOR_IDS" %in% colnames(sif)) {
      stop("SIF is missing MEDIATOR_IDS for mediator_ids filtering.")
    }
    results <- search_list_of_vectors(mediator_ids, sif$MEDIATOR_IDS)
    idx_mediator_ids <- unique(unlist(results))
    idx_list[["mediator_ids"]] <- idx_mediator_ids
  }

  if (!is.null(edge_list)) {
    edge_list <- as.data.frame(edge_list, stringsAsFactors = FALSE)
    if (ncol(edge_list) < 2) {
      stop("edge_list must have at least two columns.")
    }
    a_idx <- which(sif$PARTICIPANT_A %in% edge_list[, 1])
    b_idx <- which(sif$PARTICIPANT_B %in% edge_list[, 2])
    idx_edge_list_1 <- intersect(a_idx, b_idx)

    idx_edge_list_2 <- integer(0)
    if (isTRUE(edge_list_check_reverse)) {
      a_idx <- which(sif$PARTICIPANT_A %in% edge_list[, 2])
      b_idx <- which(sif$PARTICIPANT_B %in% edge_list[, 1])
      idx_edge_list_2 <- intersect(a_idx, b_idx)
    }

    idx_edge_list <- c(idx_edge_list_1, idx_edge_list_2)
    idx_list[["edge_list"]] <- idx_edge_list
  }

  if (length(idx_list) == 0) {
    if (isTRUE(verbose)) {
      message("filter_sif: no filters supplied; returning input.")
    }
    return(sif)
  }

  if (length(idx_list) == 1) {
    idx <- idx_list[[1]]
  } else {
    idx <- Reduce(intersect, idx_list)
  }

  filtered_network <- sif[idx, , drop = FALSE]
  if (isTRUE(verbose)) {
    message(
      "filter_sif: retained ",
      nrow(filtered_network),
      " of ",
      nrow(sif),
      " interactions."
    )
  }

  filtered_network
}

#' Find row indices where target vectors contain query values.
#'
#' @param query_values Character vector of query values.
#' @param target_values Character vector of target values (possibly delimited).
#'
#' @return List of index vectors per query.
#' @keywords internal
search_list_of_vectors <- function(query_values, target_values) {
  if (length(query_values) == 0) {
    return(list())
  }
  if (length(target_values) == 0) {
    return(rep(list(integer(0)), length(query_values)))
  }

  target_values <- ifelse(is.na(target_values), "", as.character(target_values))
  target_tokens <- strsplit(target_values, "\\s*;\\s*|\\s*,\\s*|\\s*\\|\\s*")
  target_tokens <- lapply(target_tokens, function(tokens) trimws(tokens[tokens != ""]))

  results <- vector("list", length(query_values))
  for (i in seq_along(query_values)) {
    query <- trimws(query_values[i])
    if (!nzchar(query)) {
      results[[i]] <- integer(0)
      next
    }
    results[[i]] <- which(
      vapply(target_tokens, function(tokens) query %in% tokens, logical(1))
    )
  }
  results
}
