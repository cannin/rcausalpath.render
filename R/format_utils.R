#' Convert a value into an RGB string using a blue-white-red scale.
#'
#' @param value Numeric value to map.
#' @param value_min Minimum of the scale range.
#' @param value_max Maximum of the scale range.
#'
#' @return Space-delimited RGB string.
#' @keywords internal
value_to_rgb <- function(value, value_min = -2, value_max = 2) {
  value <- max(min(value, value_max), value_min)
  if (value <= 0) {
    t <- (value - value_min) / (0 - value_min)
    r <- round(255 * t)
    g <- round(255 * t)
    b <- 255
  } else {
    t <- value / value_max
    r <- 255
    g <- round(255 * (1 - t))
    b <- round(255 * (1 - t))
  }
  sprintf("%d %d %d", r, g, b)
}

#' Build a modification map keyed by target node.
#'
#' @param edges Data.frame from read_causalpath_sif.
#'
#' @return Named list mapping node -> list(mods, is_phospho).
#' @keywords internal
build_mod_map <- function(edges) {
  mods_by_target <- split(edges$target_mods, edges$target)
  mod_map <- list()
  for (target in names(mods_by_target)) {
    mod_str <- paste(mods_by_target[[target]], collapse = ";")
    mod_list <- trimws(unlist(strsplit(mod_str, ";", fixed = TRUE)))
    mod_list <- mod_list[mod_list != ""]
    mod_list <- unique(mod_list)
    phospho_flags <- rep(FALSE, length(mod_list))
    target_rows <- edges[edges$target == target, , drop = FALSE]
    for (i in seq_along(mod_list)) {
      mod_id <- mod_list[i]
      mod_hits <- target_rows[grepl(mod_id, target_rows$target_mods, fixed = TRUE), , drop = FALSE]
      if (nrow(mod_hits) > 0 && any(mod_hits$edge_type == "phosphorylates")) {
        phospho_flags[i] <- TRUE
      }
    }
    mod_map[[target]] <- list(mods = mod_list, is_phospho = phospho_flags)
  }
  mod_map
}

#' Generate a format TSV file with random colors for nodes and modifications.
#'
#' @param input_sif Path to the input SIF file.
#' @param output_format Output format TSV path.
#' @param seed Random seed for reproducibility.
#' @param value_min Minimum random value.
#' @param value_max Maximum random value.
#'
#' @return NULL. Writes the format file to disk.
#' @export
write_format_file <- function(
    input_sif,
    output_format,
    seed = 42,
    value_min = -2,
    value_max = 2
  ) {
  edges <- read_causalpath_sif(input_sif)
  nodes <- sort(unique(c(edges$source, edges$target)))
  nodes <- nodes[nodes != ""]
  mod_map <- build_mod_map(edges)

  set.seed(seed)
  node_values <- stats::runif(length(nodes), min = value_min, max = value_max)

  rows <- list()
  rows[[1]] <- c("node", "all-nodes", "color", "255 255 255")

  for (i in seq_along(nodes)) {
    rows[[length(rows) + 1]] <- c(
      "node",
      nodes[i],
      "color",
      value_to_rgb(node_values[i], value_min, value_max)
    )
  }

  mod_nodes <- names(mod_map)
  for (node_id in mod_nodes) {
    mods <- mod_map[[node_id]]$mods
    phospho_flags <- mod_map[[node_id]]$is_phospho
    if (length(mods) == 0) {
      next
    }
    mod_values <- stats::runif(length(mods), min = value_min, max = value_max)
    for (i in seq_along(mods)) {
      mod_id <- mods[i]
      mod_label <- if (isTRUE(phospho_flags[i])) "P" else ""
      mod_rgb <- value_to_rgb(mod_values[i], value_min, value_max)
      mod_format <- paste(mod_id, mod_label, mod_rgb, sep = "|")
      rows[[length(rows) + 1]] <- c("node", node_id, "modification", mod_format)
    }
  }

  out <- do.call(rbind, rows)
  utils::write.table(
    out,
    file = output_format,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}
