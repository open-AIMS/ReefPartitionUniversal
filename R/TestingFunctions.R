fast_balanced_cut <- function(hclust_obj, target_k, min_size_ratio = 0.7, max_iter = 10) {
  n <- length(hclust_obj$order)
  target_size <- n / target_k
  
  # Start with direct k cut (fastest)
  clusters <- stats::cutree(hclust_obj, k = target_k)
  tab <- table(clusters)
  
  # Quick balance check
  sizes <- as.numeric(tab)
  cv <- sd(sizes) / mean(sizes)
  
  # If already balanced enough (CV < 0.5), return immediately
  if (cv < 0.5 && all(sizes >= target_size * min_size_ratio)) {
    return(clusters)
  }
  
  # Fast post-processing: merge small clusters
  too_small <- which(sizes < target_size * min_size_ratio)
  
  if (length(too_small) > 0) {
    # Sort small clusters by size (smallest first)
    too_small <- too_small[order(sizes[too_small])]
    
    for (ts in too_small) {
      if (sizes[ts] == 0) next
      
      # Find closest cluster (by minimum distance in dendrogram)
      # Fast heuristic: merge with cluster that has most similar merge height
      
      # Get all clusters except current
      other_clusters <- setdiff(1:length(tab), ts)
      
      # Simple: merge with largest cluster (fastest)
      largest <- other_clusters[which.max(sizes[other_clusters])]
      
      clusters[clusters == ts] <- largest
      sizes[largest] <- sizes[largest] + sizes[ts]
      sizes[ts] <- 0
    }
    
    # Renumber (fast)
    clusters <- as.integer(factor(clusters))
  }
  
  return(clusters)
}



# Use the same edges/links from constr.hclust
fast_constrained_balanced_cut <- function(hclust_obj, edges, target_k, min_size_ratio = 0.5) {
  n <- length(hclust_obj$order)
  target_size <- n / target_k
  
  # Initial cut
  clusters <- stats::cutree(hclust_obj, k = target_k)
  tab <- table(clusters)
  sizes <- as.numeric(tab)
  
  # Convert edges to adjacency list
  adj_list <- vector("list", n)
  for (i in 1:nrow(edges)) {
    adj_list[[edges[i, 1]]] <- c(adj_list[[edges[i, 1]]], edges[i, 2])
    adj_list[[edges[i, 2]]] <- c(adj_list[[edges[i, 2]]], edges[i, 1])
  }
  
  # Merge small clusters only with connected neighbors
  too_small <- which(sizes < target_size * min_size_ratio)
  
  for (ts in too_small) {
    if (sizes[ts] == 0) next
    
    # Find points in this cluster
    ts_points <- which(clusters == ts)
    
    # Find neighboring clusters (through edge connections)
    neighbor_clusters <- integer(0)
    
    for (pt in ts_points) {
      neighbors <- adj_list[[pt]]
      if (length(neighbors) > 0) {
        neighbor_clusters <- c(neighbor_clusters, 
                               clusters[neighbors])
      }
    }
    
    neighbor_clusters <- setdiff(unique(neighbor_clusters), ts)
    
    if (length(neighbor_clusters) > 0) {
      # Merge with largest connected neighbor
      neighbor_sizes <- sizes[neighbor_clusters]
      largest_neighbor <- neighbor_clusters[which.max(neighbor_sizes)]
      
      clusters[clusters == ts] <- largest_neighbor
      sizes[largest_neighbor] <- sizes[largest_neighbor] + sizes[ts]
      sizes[ts] <- 0
    } else {
      # Isolated cluster - force merge with largest overall
      # largest_cluster <- which.max(sizes)
      # if (largest_cluster != ts) {
      #   clusters[clusters == ts] <- largest_cluster
      #   sizes[largest_cluster] <- sizes[largest_cluster] + sizes[ts]
      #   sizes[ts] <- 0
      #}
    }
  }
  
  clusters <- as.integer(factor(clusters))
  return(clusters)
}


# Function to scale dissimilarity matrix to [0, 1]
scale_dissimilarity <- function(d) {
  # Convert to matrix if it's a dist object
  if (inherits(d, "dist")) {
    d <- as.matrix(d)
  }
  
  min_val <- min(d, na.rm = TRUE)
  max_val <- max(d, na.rm = TRUE)
  
  # Apply min-max scaling
  scaled_d <- (d - min_val) / (max_val - min_val)
  
  # Return as dist object if input was dist
  if (inherits(d, "dist")) {
    return(as.dist(scaled_d))
  }
  return(scaled_d)
}




