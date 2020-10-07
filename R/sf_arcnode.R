#' Create linestrings from a polygon layer
#'
#' The arcs are shared boundaries or standalone boundaries from polygons.
#'
#' The feature identity is stored in a list of data frames (the row id of the input feature).
#'
#' Create lines from shared boundaries between polygons. A single copy of a shared boundary is
#' created as 'sf' lines, keeping the row identity of the input polygon/s. It's expected that polygons have
#' clean neighbour boundaries and don't overlap.
#'
#' @param x sf polygons
#'
#' @return sf object of LINESTRING
#' @export
#' @importFrom tibble tibble
#' @importFrom sf st_set_crs st_crs
#' @importFrom silicate ARC
#' @examples
#' f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' sfx <- sf::read_sf(f)
#' arcs <- sf_arcnode(sfx)
#' plot(arcs["arc"], col = sample(hcl.colors(nrow(arcs))))
#'
#' s <- sample(seq_len(dim(sfx)[1L]), 1L)
#' plot(sfx[arcs$feature_ids[[s]]$row, 1], reset = FALSE, col = c("grey", "grey10"))
#' plot(arcs[s, 1], add = TRUE, col = "hotpink2", lwd = 6)
#'
#' plot(sfx$geom, col = sample(grey.colors(10), 100, replace = TRUE), reset = FALSE)
#' plot(dplyr::sample_n(arcs[1L], 20), col = "hotpink2", lwd = 6, add = TRUE)
sf_arcnode <- function(x) {
  ##if (!grepl("POLYGON", sf::st_geometry_type(x))) message("sf_arcnode is designed for polygon layers, behaviour on other types ")
  sc <- silicate::ARC(x)
  out <-
    sfheaders::sf_linestring(dplyr::inner_join(sc$arc_link_vertex , sc$vertex, "vertex_"), x = "x_", y = "y_", linestring_id = "arc_")
  ## list column for feature_id (the row number from x)
  out$feature_ids <- unname(lapply(split(sc$object_link_arc, sc$object_link_arc$arc_)[unique(sc$object_link_arc$arc_)],
                                   function(.x) tibble::tibble(row = match(.x$object_, sc$object$object_))))
  out$arc_ <- NULL
  out$arc <- seq_len(dim(out)[1L])

  sf::st_set_crs(out, sf::st_crs(x))
}
