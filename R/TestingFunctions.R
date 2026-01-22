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

