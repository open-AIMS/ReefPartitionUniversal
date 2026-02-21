#' Cluster points together using the spdep::skater algorithm.
#'
#' @description Take a dataframe of points containing geometries of points and
#'   `additional_variable_cols` values and cluster using the skater algorithm.
#'   This clustering is performed top-down the costs of pruning each minimum
#'   spanning tree edge. Only points connected by `edges` are able to cluster
#'   together. For additional information see **insert citation/link**.
#'
#' @param points data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param n_clust integer numeric. Number of clusters in result output. (Point to
#'   cut hierarchical clustering tree). Default = (round(nrow(points) / 200))
#'   (dividing habitat into clusters containing an average of 200 points).
#' @param site_size numeric. Desired site size (area in m^2). Default = 625,000
#'   (250m x 250m).
#' @param x_col character. Name of the column holding X coordinates. Default = "X_standard".
#' @param y_col character. Name of the column holding Y coordinates. Default = "Y_standard".
#' @param habitat_col character. Column holding unique habitat values
#'   (attached to `id_col` value and site_id values on output). Default = "habitat".
#' @param id_col character. Column holding ID value for the target reef
#'   (attached to the site_id values on output). Default = "UNIQUE_ID".
#' @param additional_variable_cols character vector. Names of additional columns
#'   to contribute to the distance matrix. Default = c("depth_standard").
#' @param parallelisation character. Current option is only "Windows", using this
#'   option sets up a parallel::Cluster using detectCores() - 2 cores. This parallelises
#'   prunecost calculations within spdep::skater(). If `parallelisation` is not
#'   set to "Windows", no parallelisation will occur. Default = "Windows".
#' @param hex_resolution integer numeric. H3 hexagon resolution used in point
#'   creation.
#'
#' @return data.frame of points with allocated site_ids based on cluster outputs.
#'   `site_id` values are a combination of the `id_col` value, `habitat_col` value
#'   and the cluster allocation.
#'
#' @export
#'
reef_skater <- function(
  points,
  n_clust = round(min(10000, nrow(points)) / 200),
  site_size = 250 * 250,
  x_col = "X_standard",
  y_col = "Y_standard",
  habitat_col = "habitat",
  id_col = "UNIQUE_ID",
  additional_variable_cols = c("depth_standard"),
  parallelisation = "Windows",
  hex_resolution = 12
) {
  site_prefix <- paste(
    unique(points[, id_col, drop = TRUE]),
    unique(points[, habitat_col, drop = TRUE]),
    sep = "_"
  )
  points$npoints <- nrow(points)

  # H3 hexagon average size
  hex_size <- data.frame(
    Res = c(7:15),
    Size = c(5161293, 737327, 105332, 15047, 2149, 307.09, 43.87, 6.267, 0.895)
  )
  min_counts <- round(site_size / hex_size$Size[hex_size$Res == hex_resolution])

  # If points contains > 10,000 points interpolation should be used. This clusters only 10,000
  # randomly sampled points from points and uses nearest neighbour interpolation to assign
  # the remaining point clusters
  interpolation <- FALSE
  if (nrow(points) > 10000) {
    interpolation <- TRUE
    samplepoints <- sample(c(1:nrow(points)), 10000)
    x_old <- points
    points <- points[samplepoints, ]

    min_counts <- min_counts * (10000 / nrow(x_old))
  }

  if (nrow(points) < 1.5 * min_counts) {
    points$site_id <- as.factor(paste(site_prefix, 1, sep = "_"))
    return(points)
  }

  mst <- prepare_mst(
    points,
    additional_variable_cols = additional_variable_cols
  )

  if (parallelisation == "Windows") {
    num_cores <- parallel::detectCores(logical = FALSE) - 2L
    spdep::set.coresOption(num_cores)
    spdep::set.mcOption(FALSE)

    cl <- parallel::makeCluster(spdep::get.coresOption())

    spdep::set.ClusterOption(cl)
  }

  # Clustering minimum spanning tree
  clusters <- spdep::skater(
    edges = igraph::as_edgelist(mst),
    data = points[, additional_variable_cols, drop = TRUE],
    ncuts = n_clust,
    crit = c(min_counts, Inf)
  ) # this seems quite intensive in terms of time

  if (parallelisation == "Windows") {
    spdep::set.ClusterOption(NULL)
    parallel::stopCluster(cl)
  }

  skater_sites <- as.factor(paste(site_prefix, clusters$groups, sep = "_"))

  if (interpolation == TRUE) {
    skater_sites <- class::knn(
      points[, c(x_col, y_col), drop = TRUE],
      x_old[, c(x_col, y_col), drop = TRUE],
      skater_sites
    )
    points <- x_old
  }

  points$site_id <- skater_sites

  points
}
