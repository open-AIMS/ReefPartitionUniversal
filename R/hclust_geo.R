#' Cluster points using standard hierarchical clustering on geographic
#' distance only (no depth, no spatial contiguity constraint).
#'
#' @description Take a dataframe of points and cluster them using
#'   `stats::hclust()` on a geographic (X/Y) distance matrix only - no depth
#'   or other additional variable contributes to the distance matrix, and no
#'   minimum spanning tree is built. Unlike `constrained_hclust()` /
#'   `constrained_hclust_mst()`, no `links`/contiguity graph is used either:
#'   this is plain, unconstrained agglomerative clustering, so at each merge
#'   step any two clusters can be joined purely because they are close in
#'   X/Y distance, regardless of what (if anything) lies between them.
#'   Resulting sites are therefore not guaranteed to be spatially contiguous,
#'   unlike every other clustering function in this package.
#'
#' @param points sf data.frame. Contains geometries and `x_col`/`y_col`
#'   coordinate columns.
#' @param x_col character. Name of the column holding X coordinates. Default = "X_standard".
#' @param y_col character. Name of the column holding Y coordinates. Default = "Y_standard".
#' @param id_col character. Column holding ID value for the target reef
#'   (attached to the site_id values on output). Default = "UNIQUE_ID".
#' @param habitat_col character. Column holding unique habitat values
#'   (attached to `id_col` value and site_id values on output). Default = "habitat".
#' @param distance_method character. Distance matrix creation method for the
#'   geographic coordinates. Default = "euclidean" (see `dist()`).
#' @param n_points integer numeric. Desired number of points per cluster.
#'   Used to calculate `n_clust` (number of output clusters). Default = 204.
#' @param n_clust integer numeric. Number of clusters in result output.
#'   (Point to cut hierarchical clustering tree). Default =
#'   `round(nrow(points) / n_points)`.
#' @param method character. Agglomeration method passed to `stats::hclust()`.
#'   Default = "ward.D2".
#' @param interpolation_threshold numeric. Habitats with more points than
#'   this are subsampled to `interpolation_threshold` points for clustering,
#'   then remaining points are assigned via nearest-neighbour interpolation -
#'   same handling `constrained_hclust_mst()` uses for large habitats.
#'   Default = 30000.
#'
#' @return data.frame of points with allocated site_ids based on cluster
#'   outputs. `site_id` values are a combination of the `id_col` value,
#'   `habitat_col` value and the cluster allocation.
#'
#' @export
#'
hclust_geo <- function(
  points,
  x_col = "X_standard",
  y_col = "Y_standard",
  id_col = "UNIQUE_ID",
  habitat_col = "habitat",
  distance_method = "euclidean",
  n_points = 204,
  n_clust = round(nrow(points) / n_points),
  method = "ward.D2",
  interpolation_threshold = 30000
) {
  site_prefix <- paste(
    unique(points[, id_col, drop = TRUE]),
    unique(points[, habitat_col, drop = TRUE]),
    sep = "_"
  )

  # Points contains over interpolation_threshold observations - cluster a
  # random subsample and assign the remaining points via nearest-neighbour
  # interpolation, same handling constrained_hclust_mst() uses.
  interpolation <- FALSE
  if (nrow(points) > interpolation_threshold) {
    interpolation <- TRUE
    samplepoints <- sample(seq_len(nrow(points)), interpolation_threshold)
    x_old <- points
    points <- points[samplepoints, ]
    n_clust <- round(nrow(points) / n_points)
  }

  coordinates <- sf::st_drop_geometry(points[, c(x_col, y_col)])
  D_geo <- dist(coordinates, method = distance_method)

  res_hclust <- stats::hclust(D_geo, method = method)
  hclust_sites <- stats::cutree(res_hclust, k = n_clust)
  hclust_sites <- as.factor(paste(site_prefix, hclust_sites, sep = "_"))

  points$site_id <- hclust_sites
  points$npoints <- nrow(points)

  if (interpolation) {
    hclust_sites <- class::knn(
      points[, c(x_col, y_col), drop = TRUE],
      x_old[, c(x_col, y_col), drop = TRUE],
      points$site_id
    )
    points <- x_old
    points$site_id <- hclust_sites
    points$npoints <- nrow(points)
  }

  points
}
