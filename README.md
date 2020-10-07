
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfarc.node

<!-- badges: start -->

[![R build
status](https://github.com/mdsumner/sfarc.node/workflows/R-CMD-check/badge.svg)](https://github.com/mdsumner/sfarc.node/actions)
<!-- badges: end -->

The goal of sfarc.node is to extract unique shared boundaries (**arcs**)
from sf polygons as sf lines.

## Installation

You can install the released version of sfarc.node from
[Github](https://github.com/mdsumner/sfarc.node) with:

``` r
remotes::install_github("mdsumner/sfarc.node")
```

## Example

This shows examples of getting these shared boundaries.

``` r
library(sfarc.node)

f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
sfx <- sf::read_sf(f)
arcs <- sf_arcnode(sfx)
plot(arcs["arc"], col = sample(hcl.colors(nrow(arcs))))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
s <- sample(seq_len(dim(sfx)[1L]), 1L)
plot(sfx[arcs$feature_ids[[s]]$row, "geom"], reset = FALSE, col = c("grey", "grey10"))
plot(arcs[s, 1], add = TRUE, col = "hotpink2", lwd = 6)
```

<img src="man/figures/README-individual-1.png" width="100%" />

``` r
#'
plot(sfx$geom, col = sample(grey.colors(10), 100, replace = TRUE), reset = FALSE)
plot(dplyr::sample_n(arcs[1], 20), col = "hotpink2", lwd = 6, add = TRUE)
```

<img src="man/figures/README-individual-2.png" width="100%" />

-----

## Code of Conduct

Please note that the sfarc.node project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
