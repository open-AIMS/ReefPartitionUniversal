reef_skater <- function(
    x, 
    n_clust = round(minimum(10000, nrow(x)) / 200), 
    x_col = "X_standard", 
    y_col = "Y_standard", 
    habitat_col = "habitat", 
    id_col = "UNIQUE_ID", 
    additional_variable_cols = c("Depth_standard"), 
    parallelisation="Windows", 
    resolution = 12
) {
    site_prefix <- paste(unique(x[, id_col]), unique(x[, habitat_col]), sep="_")
    x <- st_drop_geometry(x)

    # H3 hexagon average size
    hex_size <- data.frame(
        Res = c(7:15),
        Size = c(5161293, 737327, 105332, 15047, 2149, 307.09, 43.87, 6.267, 0.895)
    )
    min_counts <- round(site_size / hex_size$Size[hex_size$Res == resolution])

    # If x contains > 10,000 pixels interpolation should be used. This clusters only 10,000
    # randomly sampled pixels from x and uses nearest neighbour interpolation to assign
    # the remaining pixel clusters
  interpolation <- FALSE
  if (nrow(x) > 10000) {
    interpolation <- TRUE
    samplepoints <- sample(c(1:nrow(x)), 10000)
    x_old <- x
    x <- x[samplepoints, ]

    min_counts <- min_counts * (10000 / nrow(x_old))
  }

    if (nrow(x) < 1.5 * min_counts) {
        x$site_id <- as.factor(paste(site_prefix, 1, sep = "_"))
        return(x)
    }

    mst <- prepare_mst(x, additional_variable_cols = additional_variable_cols)

    if (parallelisation == "Windows") {
        num_cores <- parallel::detectCores(logical = FALSE) - 2L
        spdep::set.coresOption(num_cores)
        spdep::set.mcOption(FALSE)

        cl <- parallel::makeCluster(spdep::get.coresOption())

        spdep::set.ClusterOption(cl)
    }
    
    # Clustering minimum spanning tree
    clusters <- skater(edges = as_edgelist(mst), data = x[, additional_variable_cols], ncuts = n_clust, crit = c(min_counts, Inf)) # this seems quite intensive in terms of time

    if (parallelisation == "Windows") {
        spdep::set.ClusterOption(NULL)
        parallel::stopCluster(cl)
    }

    skater_sites <- as.factor(paste(site_prefix, clusters$groups, sep = "_"))

    if (interpolation == TRUE) {
      skater_sites <- class::knn(
        data.frame(x)[, c(x_col, y_col)],
        data.frame(x_old)[, c(x_col, y_col)], 
        skater_sites
      )
      x <- x_old
    }

    x$site_id <- skater_sites
    x$npixels <- nrow(x)

 x
}
