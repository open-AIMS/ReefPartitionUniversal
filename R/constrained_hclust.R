#' Cluster points together using the adespatial::constr.hclust algorithm.
#'
#' @description Take a dataframe of points containing `additional_variable_cols`
#'   values and edges between points in the dataframe and cluster using the
#'   constr.hclust algorithm. This clustering is performed bottom-up based on a
#'   combined distance matrix of geographic and `additional_variable_cols` distances
#'   Only points connected by `edges` are able to cluster together. For additional
#'   information see **insert citation/link**. If points contains over 30,000
#'   observations then interpolation will be used to reduce RAM usage. This process
#'   randomly samples 30,000 for clustering and assigns clusters to the remaining
#'   points using nearest neighbour interpolation.
#'
#' @param points data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param edges matrix. Matrix containing edges between points in `points`.
#' @param x_col character. Name of the column holding X coordinates. Default = "X_standard".
#' @param y_col character. Name of the column holding Y coordinates. Default = "Y_standard".
#' @param additional_variable_cols character vector. Names of additional columns
#'   to contribute to the distance matrix. Default = c("depth_standard").
#' @param id_col character. Column holding ID value for the target reef
#'   (attached to the site_id values on output). Default = "UNIQUE_ID".
#' @param habitat_col character. Column holding unique habitat values
#'   (attached to `id_col` value and site_id values on output). Default = "habitat".
#' @param distance_method character. Distance matrix creation method. Default =
#'   "manhattan" (see dist()).
#' @param distance_alpha float numeric. Weighting applied to `additional_variable_cols`
#'   distance matrix when combining with geographic distances. (1 - alpha) weighting
#'   is applied to the geographic distance matrix. Default = 0.5 (symmetric weighting).
#' @param beta float numeric. Beta parameter used by adespatial::constr.hclust.
#'   Parameter value is only used if `method` == "flexible". Default = -1.
#' @param n_points integer numeric. Desired number of points in resulting clusters.
#'   Used to calculate n_clust (number of output clusters). Value only used in
#'   n_clust specification. Default = 204.
#' @param n_clust integer numeric. Number of clusters in result output. (Point to
#'   cut hierarchical clustering tree). Default = (round(nrow(points) / n_points))
#'   (dividing habitat into clusters containing an average of 200 points).
#' @param method character. Clustering method to be applied. See adespatial::constr.hclust()
#'   for more details. Default = "ward.D2".
#'
#' @return data.frame of points with allocated site_ids based on cluster outputs.
#'   `site_id` values are a combination of the `id_col` value, `habitat_col` value
#'   and the cluster allocation.
#'
#' @export
#'
constrained_hclust <- function(
  points,
  edges,
  x_col = "X_standard",
  y_col = "Y_standard",
  additional_variable_cols = c("depth_standard"),
  id_col = "UNIQUE_ID",
  habitat_col = "habitat",
  distance_method = "manhattan",
  distance_alpha = 0.5,
  beta = -1,
  n_points = 204,
  n_clust = (round(nrow(points) / n_points)),
  method = "ward.D2"
) {
  site_prefix <- paste(
    unique(points[, id_col, drop = TRUE]),
    unique(points[, habitat_col, drop = TRUE]),
    sep = "_"
  )

  coordinates <- sf::st_drop_geometry(points[, c(x_col, y_col)])

  # Calculate weights for combining the distance matrices
  additional_variable_weight <- distance_alpha
  geo_weight <- 1 - distance_alpha

  # Calculate the raw distance matrices for additional variables and geographic distance
  D_additional_vars <- dist(
    points[, additional_variable_cols, drop = TRUE],
    method = distance_method
  )
  D_geo <- dist(coordinates)

  # Create the combined geographical and additional variable distance matrix for clustering
  D_combined <- (additional_variable_weight * D_additional_vars) +
    (geo_weight * D_geo)

  # Apply constr.hclust clustering algorithm from package adespatial.
  res_hclust <- adespatial::constr.hclust(
    d = D_combined,
    method = method,
    beta = beta,
    links = edges,
    coords = coordinates
  )
  hclust_sites <- stats::cutree(res_hclust, k = n_clust)

  hclust_sites <- as.factor(paste(site_prefix, hclust_sites, sep = "_"))

  points$site_id <- hclust_sites
  points$npoints <- nrow(points)
  points
}

#' Default habitat clustering function using adespatial::constr.hclust.
#'
#' @description Take a dataframe of points containing `additional_variable_cols`
#'   values, create a minimum spanning tree using `prepare_mst_edges()` and then
#'   cluster points using point data and edges with `constrained_hclust()`.
#'   Any additional arguments for `prepare_mst_edges()` or `constrained_hclust()`
#'   (excluding `distance_alpha`) can be included.
#'
#' @param points data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param distance_alpha float numeric. Weighting applied to the additional variable
#'   distance values when creating the distance matrix for clustering. This argument
#'   is not included in `...` for discoverability.
#' @param ... additional arguments. Additional arguments can be used here and will
#'   be passed onto `prepare_mst_edges()` and `constrained_hclust()` functions.
#'   These arguments must be named. `distance_alpha` argument is not included in
#'   these additional arguments. For information on arguments available in these
#'   functions and default values when arguments are not used, see `prepare_mst_edges()`
#'   and `constrained_hclust()`.
#'
#' @return data.frame of points with allocated site_ids based on cluster outputs
#'   from `constrained_hclust()` using `prepare_mst_edges` to create a minimum
#'   spanning tree for input. `site_id` values are a combination of the `id_col`
#'   value, `habitat_col` value and the cluster allocation.
#'
#' @export
#'
constrained_hclust_mst <- function(
  points,
  distance_alpha = 0.5,
  n_points = 204,
  x_col = "X_standard",
  y_col = "Y_standard",
  ...
) {
  dots <- list(...)
  passed_arguments <- names(dots)

  mst_params <- dots[passed_arguments %in% names(formals(prepare_mst))]
  constrained_clust_params <- dots[
    passed_arguments %in% names(formals(constrained_hclust))
  ]

  interpolation <- FALSE
  if (nrow(points) > 30000) {
    interpolation <- TRUE
    samplepoints <- sample(c(1:nrow(points)), 30000)
    x_old <- points
    points <- points[samplepoints, ]

    n_clust <- round(nrow(points) / n_points)
    constrained_clust_params["n_clust"] <- n_clust

    # min_counts <- min_counts * (30000 / nrow(x_old))
  }

  mst <- do.call(prepare_mst, append(list(points = points), mst_params))
  mst_edges <- igraph::as_edgelist(mst)

  # Extract clust_ prefixed args and strip the prefix

  clustered_points <- do.call(
    constrained_hclust,
    append(
      list(points = points, edges = mst_edges, distance_alpha = distance_alpha),
      constrained_clust_params
    )
  )

  if (interpolation == TRUE) {
    hclust_sites <- class::knn(
      clustered_points[, c(x_col, y_col), drop = TRUE],
      x_old[, c(x_col, y_col), drop = TRUE],
      clustered_points$site_id
    )
    clustered_points <- x_old

    clustered_points$site_id <- hclust_sites
    clustered_points$npoints <- nrow(clustered_points)
  }

  return(clustered_points)
}
