#' Create a minimum spanning tree from geographic coordinates of points and extracted
#' data.
#'
#' @description Take a dataframe of points `points` and create a minimum spanning tree
#'   between pixel coordinates, using edge costs that are a combination of geographic
#'   distances and distances between points by `additional_variable_cols` values.
#'   For additional details on minimum spanning tree creation see igraph::mst().
#'
#' @param points sf data.frame. Holds values for pixel geometries and
#'   `additional_variable_cols`
#' @param additional_variable_cols character vector. Names of the columns to extract
#'   additional (non-geometric) data from for cost weighting.
#' @param mst_alpha float numeric. Weighting applied to `additional_variable_cols`
#'   distance in edge cost weighting when combining with geographic distances.
#'   (1 - alpha) weight is applied to the geographic distance. Default = 0.5
#'   (same weight for `additional_variable_cols` and geographic distances).
#' @param hex_resolution integer. H3 hex resolution of pixels. Default = 12.
#'
#' @return igraph::mst Minimum spanning tree object.
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
prepare_mst <- function(
  points,
  additional_variable_cols = c("depth_standard"),
  mst_alpha = 0.5,
  hex_resolution = 12
) {
  add_var_weight <- mst_alpha
  geo_weight <- 1 - mst_alpha
  coords <- sf::st_centroid(sf::st_geometry(points))

  # Triangulate edges between pixel points
  tri <- spdep::tri2nb(coords)
  Costs_tri <- spdep::nbcosts(
    tri,
    data = points[, additional_variable_cols, drop = TRUE],
    method = "manhattan"
  )
  Costs_tri <- unlist(Costs_tri)

  Edges_tri <- neighborsDataFrame(tri)

  Edges_tri2 <- data.frame(
    from = as.numeric(Edges_tri$id),
    to = as.numeric(Edges_tri$id_neigh),
    weights = Costs_tri
  )
  Network_withEdgesTri <- sfnetworks::sfnetwork(
    points,
    Edges_tri2[, c(1, 2)],
    directed = FALSE
  )
  igraph::E(Network_withEdgesTri)$weight <- Costs_tri

  Network_withEdgesTri <- Network_withEdgesTri %>%
    tidygraph::activate("edges") %>%
    mutate(length = sfnetworks::edge_length())

  Length_scaled <- scale(as.numeric(igraph::E(Network_withEdgesTri)$length))
  Weight_scaled <- scale(as.numeric(igraph::E(Network_withEdgesTri)$weight))

  Eucliden_weight_old <- sqrt(
    (Length_scaled^2) * geo_weight + (Weight_scaled^2) * add_var_weight
  )
  igraph::E(Network_withEdgesTri)$weight <- Eucliden_weight_old

  # Triangulation
  mst_tri <- igraph::mst(
    Network_withEdgesTri,
    weights = igraph::E(Network_withEdgesTri)$weight
  ) #+Length_scaled)

  mst_tri
}

#' Convert an spdep::nb neighbors object into a dataframe with columns `id`
#' and `id_neigh`, one row per (node, neighbor) pair, in the same natural
#' per-node order that `spdep::nbcosts()` produces its cost vector in.
#'
#' Originally adapted from package `expp` (function copied on 2025-01-20 to
#' avoid an `expp` dependency, as that package is no longer maintained).
#' Reimplemented with direct vectorized indexing (instead of `expp`'s
#' `merge()`-based join) because `merge()` sorts its output by join key,
#' which silently reordered rows relative to `nb`'s natural order and desynced
#' this function's output from `spdep::nbcosts()`'s output when the two were
#' zipped together by position in `prepare_mst()`.
neighborsDataFrame <- function(nb) {
  stopifnot(inherits(nb, "nb"))

  k <- unlist(mapply(rep, seq_along(nb), sapply(nb, length), SIMPLIFY = FALSE))
  k_nb <- unlist(nb)
  region_id <- attributes(nb)$region.id

  data.frame(id = region_id[k], id_neigh = region_id[k_nb])
}
