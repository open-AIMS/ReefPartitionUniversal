#' Cluster pixels together using the spdep::skater algorithm.
#' 
#' @description Take a dataframe of pixels containing geometries of pixels and 
#'   `additional_variable_cols` values and cluster using the skater algorithm. 
#'   This clustering is performed top-down the costs of pruning each minimum 
#'   spanning tree edge. Only pixels connected by `edges` are able to cluster 
#'   together. For additional information see **insert citation/link**.
#' 
#' @param pixels data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param n_clust integer numeric. Number of clusters in result output. (Point to
#'   cut hierarchical clustering tree). Default = (round(nrow(pixels) / 200))
#'   (dividing habitat into clusters containing an average of 200 pixels).
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
#' @param hex_resolution integer numeric. H3 hexagon resolution used in pixel
#'   creation.
#' 
#' @return data.frame of pixels with allocated site_ids based on cluster outputs.
#'   `site_id` values are a combination of the `id_col` value, `habitat_col` value
#'   and the cluster allocation.
#' 
#' @export
#' 
reef_skater <- function(
    pixels, 
    n_clust = round(min(10000, nrow(pixels)) / 200), 
    site_size = 250 * 250,
    x_col = "X_standard", 
    y_col = "Y_standard", 
    habitat_col = "habitat", 
    id_col = "UNIQUE_ID", 
    additional_variable_cols = c("depth_standard"), 
    parallelisation="Windows", 
    hex_resolution = 12
) {
    site_prefix <- paste(unique(pixels[, id_col, drop = TRUE]), unique(pixels[, habitat_col, drop = TRUE]), sep="_")
    pixels$npixels <- nrow(pixels)

    # H3 hexagon average size
    hex_size <- data.frame(
        Res = c(7:15),
        Size = c(5161293, 737327, 105332, 15047, 2149, 307.09, 43.87, 6.267, 0.895)
    )
    min_counts <- round(site_size / hex_size$Size[hex_size$Res == hex_resolution])

    # If pixels contains > 10,000 pixels interpolation should be used. This clusters only 10,000
    # randomly sampled pixels from pixels and uses nearest neighbour interpolation to assign
    # the remaining pixel clusters
  interpolation <- FALSE
  if (nrow(pixels) > 10000) {
    interpolation <- TRUE
    samplepoints <- sample(c(1:nrow(pixels)), 10000)
    x_old <- pixels
    pixels <- pixels[samplepoints, ]

    min_counts <- min_counts * (10000 / nrow(x_old))
  }

    if (nrow(pixels) < 1.5 * min_counts) {
        pixels$site_id <- as.factor(paste(site_prefix, 1, sep = "_"))
        return(pixels)
    }

    mst <- prepare_mst(pixels, additional_variable_cols = additional_variable_cols)

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
        data = pixels[, additional_variable_cols, drop = TRUE], 
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
        pixels[, c(x_col, y_col), drop = TRUE],
        x_old[, c(x_col, y_col), drop = TRUE], 
        skater_sites
      )
      pixels <- x_old
    }

    pixels$site_id <- skater_sites

 pixels
}
