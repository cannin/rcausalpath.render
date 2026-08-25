# rcausalpath

`rcausalpath` renders CausalPath SIF networks using base R graphics with optional
format-driven coloring and igraph layouts when available.

## Contents

- `R/plot_causalpath.R`: Core plotting and parsing helpers.
- `R/format_utils.R`: Format TSV generation helpers.
- `inst/extdata/`: Example SIF files.
- `vignettes/`: Walkthrough using the sample data.

## Quick start

```r
library(rcausalpath)

sif_path <- system.file("extdata", "causalpath_causative_small.sif", package = "rcausalpath")
format_path <- tempfile(fileext = "_format.tsv")

write_format_file(sif_path, format_path)
plot_causalpath(sif_path, format_file = format_path, layout_method = "layout_with_sugiyama")
```

## Notes

- If `igraph` is installed, force-directed layouts are available. Otherwise, a circular layout is used.
- Outputs are written to PNG and SVG by default; pass `output_png = NULL` or `output_svg = NULL` to skip.
