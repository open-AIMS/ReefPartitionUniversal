constrained_hclust <- function(
    x, 
    edges, 
    x_col = "X_standard", 
    y_col = "Y_standard", 
    additional_variable_cols = c("Depth_standard"), 
    id_col = "UNIQUE_ID", 
    habitat_col = "habitat", 
    distance_method = "manhattan", 
    alpha = 0.4, 
    n_clust = (round(nrow(x) / 200))
) {
    site_prefix <- paste(unique(x[, id_col]), unique(x[, habitat_col]), sep="_")
    coordinates <- st_drop_geometry(x[, c(x_col, y_col)])

    # Calculate weights for combining the distance matrices
    additional_variable_weight <- alpha
    geo_weight <- 1 - alpha

    # Calculate the raw distance matrices for additional variables and geographic distance
    D_additional_vars <- dist(x[, additional_variable_cols], method = distance_method)
    D_geo <- dist(coordinates)

    # Create the combined geographical and additional variable distance matrix for clustering
    D_combined <- (additional_variable_weight * D_additional_vars) + (geo_weight * D_geo)

    # Apply constr.hclust clustering algorithm from package adespatial.
    res_hclust <- constr.hclust(
        d = D_combined,
        method = "flexible",
        beta = -1,
        links = edges,
        coords = coordinates
    )
    hclust_sites <- cutree(res_hclust, k = n_clust)

    hclust_sites <- as.factor(paste(site_prefix, hclust_sites, sep = "_"))
    x$site_id <- hclust_sites
    x$npixels <- nrow(x)
    x
}