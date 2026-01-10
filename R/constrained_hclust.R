#' Cluster pixels together using the adespatial::constr.hclust algorithm.
#' 
#' @description Take a dataframe of pixels containing `additional_variable_cols`
#'   values and edges between pixels in the dataframe and cluster using the
#'   constr.hclust algorithm. This clustering is performed bottom-up based on a
#'   combined distance matrix of geographic and `additional_variable_cols` distances
#'   Only pixels connected by `edges` are able to cluster together. For additional
#'   information see **insert citation/link**.
#' 
#' @param pixels data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param edges matrix. Matrix containing edges between pixels in `pixels`.
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
#' @param n_clust integer numeric. Number of clusters in result output. (Point to
#'   cut hierarchical clustering tree). Default = (round(nrow(pixels) / 200))
#'   (dividing habitat into clusters containing an average of 200 pixels).
#' @param method character. Clustering method to be applied. See adespatial::constr.hclust()
#'   for more details. Default = "ward.D2".
#' 
#' @return data.frame of pixels with allocated site_ids based on cluster outputs.
#'   `site_id` values are a combination of the `id_col` value, `habitat_col` value
#'   and the cluster allocation.
#' 
#' @export
#' 
constrained_hclust <- function(
    pixels, 
    edges, 
    x_col = "X_standard", 
    y_col = "Y_standard", 
    additional_variable_cols = c("depth_standard"), 
    id_col = "UNIQUE_ID", 
    habitat_col = "habitat", 
    distance_method = "manhattan", 
    distance_alpha = 0.4,
    beta = -1,
    n_clust = (round(nrow(pixels) / 200)),
    method = "ward.D2"
) {
    site_prefix <- paste(unique(pixels[, id_col, drop = TRUE]), unique(pixels[, habitat_col, drop = TRUE]), sep="_")

    interpolation <- FALSE
    if (nrow(pixels) > 30000) {
        interpolation <- TRUE
        samplepoints <- sample(c(1:nrow(pixels)), 30000)
        x_old <- pixels
        pixels <- pixels[samplepoints, ]

        min_counts <- min_counts * (30000 / nrow(x_old))
    }
    coordinates <- sf::st_drop_geometry(pixels[, c(x_col, y_col)])

    # Calculate weights for combining the distance matrices
    additional_variable_weight <- alpha
    geo_weight <- 1 - alpha

    # Calculate the raw distance matrices for additional variables and geographic distance
    D_additional_vars <- dist(pixels[, additional_variable_cols, drop = TRUE], method = distance_method)
    D_geo <- dist(coordinates)

    # Create the combined geographical and additional variable distance matrix for clustering
    D_combined <- (additional_variable_weight * D_additional_vars) + (geo_weight * D_geo)

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
    
    if (interpolation == TRUE) {
      hclust_sites <- class::knn(
        pixels[, c(x_col, y_col), drop = TRUE],
        x_old[, c(x_col, y_col), drop = TRUE], 
        hclust_sites
      )
      pixels <- x_old
    }

    pixels$site_id <- hclust_sites
    pixels$npixels <- nrow(pixels)
    pixels
}

#' Default habitat clustering function using adespatial::constr.hclust.
#' 
#' @description Take a dataframe of pixels containing `additional_variable_cols`
#'   values, create a minimum spanning tree using `prepare_mst_edges()` and then
#'   cluster pixels using pixel data and edges with `constrained_hclust()`.
#'   Any additional arguments for `prepare_mst_edges()` or `constrained_hclust()`
#'   (excluding `distance_alpha`) can be included.
#' 
#' @param pixels data.frame. Contains values for X and Y coordinates, as well as
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
#' @return data.frame of pixels with allocated site_ids based on cluster outputs
#'   from `constrained_hclust()` using `prepare_mst_edges` to create a minimum
#'   spanning tree for input. `site_id` values are a combination of the `id_col` 
#'   value, `habitat_col` value and the cluster allocation.
#' 
#' @export
#' 
constrained_hclust_mst <- function(pixels, distance_alpha=0.5, ...) {
    dots <- list(...)
    passed_arguments <- names(dots)
  
    mst_params <- passed_arguments[names(passed_arguments) %in% names(formals(prepare_mst_edges))]
    mst_edges <- do.call(prepare_mst_edges, c(list(pixels = pixels), mst_params))
  
    # Extract clust_ prefixed args and strip the prefix
    constrained_clust_params <- passed_arguments[names(passed_arguments) %in% names(formals(constrained_hclust))]
    clustered_pixels <- do.call(
        constrained_hclust, 
        c(list(pixels = pixel_data, edges = mst_edges, distance_alpha = alpha), constrained_clust_params)
    )

    return(clustered_pixels)
}
