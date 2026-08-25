#' Build a force-directed layout using igraph if available.
#'
#' @param edges Data.frame returned by read_causalpath_sif.
#' @param nodes Character vector of node names in desired order.
#' @param layout_method igraph layout function name.
#'
#' @return A data.frame with node, x, y.
#' @keywords internal
build_force_layout <- function(edges, nodes, layout_method = LAYOUT_METHOD) {
  valid_edges <- edges[edges$source != "" & edges$target != "", , drop = FALSE]
  graph <- igraph::graph_from_data_frame(
    valid_edges[, c("source", "target")],
    directed = TRUE,
    vertices = data.frame(name = nodes, stringsAsFactors = FALSE)
  )
  set.seed(LAYOUT_SEED)
  layout_func <- get(layout_method, envir = asNamespace("igraph"), inherits = FALSE)
  coords <- tryCatch(
    layout_func(graph),
    error = function(e) igraph::layout_with_fr(graph)
  )
  if (is.list(coords) && !is.null(coords$layout)) {
    coords <- coords$layout
  }
  rownames(coords) <- igraph::V(graph)$name
  layout <- data.frame(
    node = rownames(coords),
    x = coords[, 1],
    y = coords[, 2],
    stringsAsFactors = FALSE
  )
  layout <- layout[match(nodes, layout$node), , drop = FALSE]
  if (any(is.na(layout$node))) {
    stop("Layout is missing nodes; check SIF parsing for unexpected node names.")
  }
  layout
}
