#'  Internal helper to compute the sum of square distances within cluster nodes.
#' Follows spdep function, implementing igraph methods.
ssw <- function(
  data,
  nodes,
  method = "euclidean",
  p = 2,
  cov = NULL,
  inverted = FALSE
) {
  if (length(nodes) <= 1) {
    return(0)
  }

  cluster_data <- data[nodes, , drop = FALSE]

  if (method == "euclidean") {
    no_geom <- sf::st_drop_geometry(cluster_data)
    center <- colMeans(no_geom)

    return(sum(colSums((t(no_geom) - center)^2)))
  } else if (method == "manhattan") {
    no_geom <- sf::st_drop_geometry(cluster_data)
    center <- colMeans(no_geom)

    # Sum of absolute deviations from centroid
    return(sum(abs(sweep(no_geom, 2, center))))
  } else if (method == "mahalanobis") {
    if (is.null(cov)) {
      cov <- var(cluster_data)
    }

    if (!inverted) {
      cov <- solve(cov)
    }

    center <- colMeans(cluster_data)
    diffs <- t(cluster_data) - center

    return(sum(diag(t(diffs) %*% cov %*% diffs)))
  } else {
    dists <- dist(cluster_data, method = method, p = p)

    return(sum(dists^2) / (2 * length(nodes)))
  }
}

#' An approximate Skater algorithm implemented using igraph methods.
#'
#' @description Leverages C implementations in igraph package.
#' The approach collects all edges from all clusters, sorts by global cost, and
#' iterates until a valid split is found.
#'
skater_igraph <- function(
  edges,
  data,
  ncuts,
  crit,
  vec.crit,
  method = "euclidean",
  p = 2,
  cov = NULL,
  inverted = FALSE
) {
  n <- nrow(edges) + 1

  # Initialize by pre-computing all edge costs
  message("Pre-computing edge costs...")
  edge_costs <- numeric(nrow(edges))

  for (i in 1:nrow(edges)) {
    node1 <- edges[i, 1]
    node2 <- edges[i, 2]
    nodes_combined <- c(node1, node2)

    # Cost = increase in SSW when edge is removed
    ssw_combined <- ssw(data, nodes_combined, method, p, cov, inverted)
    ssw_separate <- ssw(data, node1, method, p, cov, inverted) +
      ssw(data, node2, method, p, cov, inverted)
    edge_costs[i] <- ssw_combined - ssw_separate
  }

  # Build initial graph with all nodes in one cluster
  g <- igraph::graph_from_edgelist(edges[, 1:2], directed = FALSE)
  igraph::E(g)$cost <- edge_costs
  igraph::E(g)$edge_id <- 1:nrow(edges) # Track original edge IDs

  # Sort edges by cost (highest cost first)
  sorted_order <- order(edge_costs, decreasing = TRUE)

  # Initialize cluster list
  ssto <- ssw(data, 1:n, method, p, cov, inverted)
  clusters <- list(list(
    nodes = 1:n,
    graph = g,
    ssw = ssto
  ))

  # Handle parameters
  if (missing(crit)) {
    crit <- c(1, Inf)
  }
  if (missing(vec.crit)) {
    vec.crit <- rep(1, n)
  }
  if (missing(ncuts)) {
    ncuts <- n - 1
  }

  # Track which clusters can't be split further
  cannot_prune <- logical(1)

  # Apply SKATER algorithm
  cuts <- 0

  repeat {
    if (cuts >= ncuts) {
      break
    }

    # Get candidate clusters (those that can still be pruned)
    candidates <- which(!cannot_prune)
    if (length(candidates) == 0) {
      break
    }

    # BUILD GLOBAL CANDIDATE LIST (original algorithm approach)
    # Collect all edges from all candidate clusters
    candidate_list <- list()

    for (cl_idx in candidates) {
      cl <- clusters[[cl_idx]]
      if (igraph::ecount(cl$graph) == 0) {
        next
      }

      # Get edges and their costs from this cluster
      edge_costs_cl <- igraph::E(cl$graph)$cost
      edge_ids_cl <- igraph::E(cl$graph)$edge_id

      if (length(edge_costs_cl) > 0) {
        # Store: cluster_id, edge_index_in_cluster, cost, original_edge_id
        for (e_idx in 1:length(edge_costs_cl)) {
          candidate_list[[length(candidate_list) + 1]] <- list(
            cluster_id = cl_idx,
            edge_idx = e_idx,
            cost = edge_costs_cl[e_idx],
            orig_edge_id = edge_ids_cl[e_idx]
          )
        }
      }
    }

    if (length(candidate_list) == 0) {
      break
    }

    # Sort all candidates globally by cost (ORIGINAL ALGORITHM)
    candidate_costs <- sapply(candidate_list, function(x) x$cost)
    global_order <- order(candidate_costs, decreasing = TRUE)
    candidate_list <- candidate_list[global_order]

    # Try edges in global order
    split_successful <- FALSE

    for (cand in candidate_list) {
      cl_idx <- cand$cluster_id
      e_idx <- cand$edge_idx

      # Skip if this cluster was already pruned in this iteration
      if (cl_idx > length(clusters)) {
        next
      }
      if (cannot_prune[cl_idx]) {
        next
      }

      cl <- clusters[[cl_idx]]

      # Test removing this edge
      test_graph <- igraph::delete_edges(cl$graph, e_idx)
      comp <- igraph::components(test_graph)

      # Check if it actually split (should be 2 components)
      if (comp$no != 2) {
        next
      }

      # Check size constraints
      comp_sizes <- sapply(1:comp$no, function(i) {
        nodes_in_comp <- cl$nodes[comp$membership == i]
        sum(vec.crit[nodes_in_comp])
      })

      # Check if constraints are satisfied
      if (any(comp_sizes < crit[1] | comp_sizes > crit[2])) {
        # Mark cluster as unprunable if ALL its edges violate constraints
        # (This is conservative - original SKATER does more bookkeeping)
        next
      }

      # Split if a valid split is found
      new_clusters <- lapply(1:comp$no, function(i) {
        nodes_in_comp <- cl$nodes[comp$membership == i]
        subgraph <- igraph::induced_subgraph(
          test_graph,
          which(comp$membership == i)
        )

        list(
          nodes = nodes_in_comp,
          graph = subgraph,
          ssw = ssw(data, nodes_in_comp, method, p, cov, inverted)
        )
      })

      # Replace old cluster with new ones
      clusters[[cl_idx]] <- new_clusters[[1]]
      clusters[[length(clusters) + 1]] <- new_clusters[[2]]

      # Update cannot_prune vector
      cannot_prune[cl_idx] <- FALSE # New cluster, can try again
      cannot_prune <- c(cannot_prune, FALSE) # New cluster can be pruned

      cuts <- cuts + 1
      split_successful <- TRUE
      break # Found valid split, restart evaluation
    }

    # If no splits were successful, mark remaining candidates as unprunable
    if (!split_successful) {
      cannot_prune[candidates] <- TRUE
    }
  }

  # Build result
  groups <- integer(n)
  for (i in 1:length(clusters)) {
    groups[clusters[[i]]$nodes] <- i
  }

  # Build edges.groups structure (for compatibility with original)
  edges_groups <- lapply(clusters, function(cl) {
    edge_matrix <- igraph::as_edgelist(cl$graph)
    if (nrow(edge_matrix) > 0) {
      # Add costs as third column
      costs <- igraph::E(cl$graph)$cost
      edge_matrix <- cbind(edge_matrix, costs)
    } else {
      edge_matrix <- matrix(0, 0, 3)
    }

    list(
      node = cl$nodes,
      edge = edge_matrix,
      ssw = cl$ssw
    )
  })

  result <- list(
    groups = groups,
    edges.groups = edges_groups,
    not.prune = which(cannot_prune),
    candidates = which(!cannot_prune),
    ssto = ssto,
    ssw = cumsum(sapply(clusters, function(x) x$ssw)),
    crit = crit,
    vec.crit = vec.crit
  )

  attr(result, "class") <- "skater"
  return(result)
}

#' Cluster points together using an optimized SKATER algorithm.
#'
#' @description Take a dataframe of points containing geometries of points and
#'   `additional_variable_cols` values and cluster using the SKATER (Spatial
#'   'K'luster Analysis by Tree Edge Removal) algorithm. This clustering is
#'   performed by iteratively pruning edges from a minimum spanning tree based
#'   on the costs of edge removal. Only points connected by edges in the MST
#'   are able to cluster together. This implementation uses a hybrid approach
#'   combining igraph graph operations with the original SKATER algorithm logic
#'   for improved performance. For datasets exceeding 10,000 points, clustering
#'   is performed on a random sample with results interpolated to remaining points
#'   via nearest neighbor assignment.
#'
#' @param points data.frame. Contains values for X and Y coordinates, as well as
#'   `additional_variable_cols`.
#' @param n_clust integer numeric. Number of clusters in result output. Default =
#'   (round(nrow(points) / 200)) (dividing habitat into clusters containing an
#'   average of 200 points).
#' @param site_size numeric. Desired site size (area in m^2). Used to calculate
#'   minimum cluster size constraint based on H3 hexagon resolution. Default =
#'   250 * 250 (62,500 m^2).
#' @param x_col character. Name of the column holding X coordinates. Default =
#'   "X_standard".
#' @param y_col character. Name of the column holding Y coordinates. Default =
#'   "Y_standard".
#' @param habitat_col character. Column holding unique habitat values
#'   (attached to `id_col` value and site_id values on output). Default = "habitat".
#' @param id_col character. Column holding ID value for the target reef
#'   (attached to the site_id values on output). Default = "UNIQUE_ID".
#' @param additional_variable_cols character vector. Names of additional columns
#'   to contribute to the distance matrix for clustering. Default = c("depth_standard").
#' @param point_area integer numeric. Area implicitly occupied by each point.
#'   Should be in the same units as `site_size`.
#'   If using pixels extracted from raster data then this value is res(raster)
#'   * res(raster), if using H3 cells then this value is the H3 cell area determined
#'   by the hexagon resolution (e.g. resolution 12 = cell area 307.2 m^2). Default
#'   point_area = 100 m^2.
#' @param method character. Distance metric for calculating dissimilarity between
#'   points. Options include "euclidean", "manhattan", "maximum", "canberra",
#'   "binary", "minkowski", "mahalanobis". Default = "euclidean".
#' @param interpolation_threshold numeric. Threshold from where to sample random
#'   points and interpolate clusters for remaining points. This value should be
#'   scaled with reef area for larger reefs. Default value is 30,000, setting a higher
#'   threshold may result in long computation times and high RAM usage.
#'
#' @return data.frame of points with allocated site_ids based on cluster outputs.
#'   `site_id` values are a combination of the `id_col` value, `habitat_col` value
#'   and the cluster allocation.
#'
#' @export
#'
reef_skater_fast <- function(
  points,
  n_clust = NA,
  site_size = 250 * 250,
  x_col = "X_standard",
  y_col = "Y_standard",
  habitat_col = "habitat",
  id_col = "UNIQUE_ID",
  additional_variable_cols = c("depth_standard"),
  point_area = 100,
  mst_alpha = 0.5,
  method = "euclidean",
  interpolation_threshold = 30000
) {
  site_prefix <- paste(
    unique(points[, id_col, drop = TRUE]),
    unique(points[, habitat_col, drop = TRUE]),
    sep = "_"
  )
  points$npoints <- nrow(points)

  min_counts <- round(site_size / point_area)

  if (is.na(n_clust)) {
    total_area <- nrow(points) * point_area
    n_clust <- ceiling(max(1, round(total_area / site_size)))
  }

  # Handle large datasets via interpolation
  interpolation <- FALSE
  if (nrow(points) > interpolation_threshold) {
    interpolation <- TRUE
    samplepoints <- sample(1:nrow(points), interpolation_threshold)
    x_old <- points
    points <- points[samplepoints, ]

    # Adjust minimum counts for sampled dataset
    min_counts <- min_counts * (interpolation_threshold / nrow(x_old))
  }

  # Early return if too few points for clustering
  if (nrow(points) < 1.5 * min_counts) {
    points$site_id <- as.factor(paste(site_prefix, 1, sep = "_"))
    if (interpolation) {
      x_old$site_id <- points$site_id[1]
      return(x_old)
    }
    return(points)
  }

  # Build minimum spanning tree
  message("Building minimum spanning tree...")
  mst <- prepare_mst(
    points,
    additional_variable_cols = additional_variable_cols,
    hex_resolution = 12,
    mst_alpha = mst_alpha
  )

  # Convert MST to edge list for skater_igraph
  edges <- igraph::as_edgelist(mst)

  # Run hybrid SKATER clustering
  message(sprintf(
    "Clustering %d points into %d groups...",
    nrow(points),
    n_clust
  ))

  clusters <- skater_igraph(
    edges = edges,
    data = points[, additional_variable_cols, drop = FALSE],
    ncuts = n_clust - 1, # ncuts is number of cuts, not final clusters
    crit = c(min_counts, Inf),
    method = method
  )

  # Assign cluster labels with site prefix
  skater_sites <- as.factor(paste(site_prefix, clusters$groups, sep = "_"))

  # Interpolate back to full dataset if needed
  if (interpolation) {
    message("Interpolating clusters to full dataset...")

    # Drop geometry if `sf` object for knn training
    if (inherits(points, "sf")) {
      points_no_geom <- sf::st_drop_geometry(points)
    } else {
      points_no_geom <- points
    }

    # Drop geometry from full dataset for knn testing
    if (inherits(x_old, "sf")) {
      x_old_no_geom <- sf::st_drop_geometry(x_old)
    } else {
      x_old_no_geom <- x_old
    }

    # Perform knn interpolation on non-geometry data
    skater_sites <- class::knn(
      train = as.matrix(points_no_geom[, c(x_col, y_col)]),
      test = as.matrix(x_old_no_geom[, c(x_col, y_col)]),
      cl = skater_sites,
      k = 1
    )

    # Add site_id to original x_old (preserves geometry if present)
    x_old$site_id <- skater_sites
    points <- x_old
  }

  points$site_id <- skater_sites

  return(points)
}
