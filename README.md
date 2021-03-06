
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfarc.node

<!-- badges: start -->

[![R build
status](https://github.com/mdsumner/sfarc.node/workflows/R-CMD-check/badge.svg)](https://github.com/mdsumner/sfarc.node/actions)
<!-- badges: end -->

The goal of sfarc.node is to extract unique shared boundaries (**arcs**)
from sf polygons as sf lines.

## Installation

You can install the dev version of sfarc.node from
[Github](https://github.com/mdsumner/sfarc.node) with:

``` r
remotes::install_github("mdsumner/sfarc.node")
```

## Example

This shows examples of getting these shared boundaries. The feature id/s
(the row number/s) from which each boundary came are listed in a list
column, so that we can look up the original polygon/s for each **arc**.
(There should be 1, or possibly 2 but results may vary wildly with
unclean data).

``` r
library(sfarc.node)

f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
sfx <- sf::read_sf(f)
arcs <- sf_arcnode(sfx)
plot(arcs["arc"], col = sample(hcl.colors(nrow(arcs))))
```

<img src="man/figures/README-example-1.png" width="100%" />

We can get the original polygon id/s for a given boundary.

``` r
s <- sample(seq_len(dim(arcs)[1L]), 1L)
plot(sfx[arcs$feature_ids[[s]]$row, "geom"], reset = FALSE, col = c("grey", "grey10"))
plot(arcs[s, 1], add = TRUE, col = "hotpink2", lwd = 6)
```

<img src="man/figures/README-individual-1.png" width="100%" />

``` r

plot(sfx$geom, col = sample(grey.colors(10), 100, replace = TRUE), reset = FALSE)
plot(dplyr::sample_n(arcs[1], 20), col = "hotpink2", lwd = 6, add = TRUE)
```

<img src="man/figures/README-individual-2.png" width="100%" />

Works fine with more intensive data.

``` r
xx <- ozmaps::abs_ced
arx <- sf_arcnode(xx)

idx <- which(unlist(lapply(arx$feature_ids, function(.x) length(.x$row) > 1)))
s <- idx[1]
plot(xx[arx$feature_ids[[s]]$row, "geometry"], reset = FALSE, col = c("grey", "grey10"))
plot(arx[s, 1], add = TRUE, col = "hotpink2", lwd = 4)
```

<img src="man/figures/README-ced-1.png" width="100%" />

``` r

for (j in 1:6) {
par(mfrow = c(5, 5), mar = rep(0, 4))
for (i in 1:25) {
s <- sample(idx, 1)
plot(xx[arx$feature_ids[[s]]$row, "geometry"], reset = FALSE, col = grey(c(0.8, 0.5)))
plot(arx[s, 1], add = TRUE, col = "hotpink1", lwd = 4)
}
}
```

<img src="man/figures/README-ced-2.png" width="100%" /><img src="man/figures/README-ced-3.png" width="100%" /><img src="man/figures/README-ced-4.png" width="100%" /><img src="man/figures/README-ced-5.png" width="100%" /><img src="man/figures/README-ced-6.png" width="100%" /><img src="man/figures/README-ced-7.png" width="100%" />

-----

## Code of Conduct

Please note that the sfarc.node project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
