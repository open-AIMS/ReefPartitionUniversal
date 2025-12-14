prepare_mst <- function(x, additional_variable_cols = c("Depth_standard")) {
  coords <- st_centroid(st_geometry(x))

  # Triangulate edges between pixel points
  tri <- tri2nb(coords)
  Costs_tri <- nbcosts(tri, data = x[, additional_variable_cols], method = "manhattan")
  Costs_tri <- unlist(Costs_tri)

  Edges_tri <- expp::neighborsDataFrame(tri)

  Edges_tri2 <- data.frame(from = as.numeric(Edges_tri$id), to = as.numeric(Edges_tri$id_neigh), weights = Costs_tri)
  Network_withEdgesTri <- sfnetwork(x, Edges_tri2[, c(1, 2)], directed = FALSE)
  E(Network_withEdgesTri)$weight <- Costs_tri

  Network_withEdgesTri <- Network_withEdgesTri %>%
    activate("edges") %>%
    mutate(length = edge_length())
  Length_scaled <- scale(as.numeric(E(Network_withEdgesTri)$length))
  Weight_scaled <- scale(as.numeric(E(Network_withEdgesTri)$weight))

  Eucliden_weight_old <- sqrt(Length_scaled^2 + Weight_scaled^2)
  E(Network_withEdgesTri)$weight <- Eucliden_weight_old

  # Triangulation
  mst_tri <- mst(Network_withEdgesTri, weights = E(Network_withEdgesTri)$weight) #+Length_scaled)

  mst_tri
}

prepare_tri_edges <- function(x) {
  coords <- st_centroid(st_geometry(x))

  # k_neighbor_list <- knearneigh(coords, k = 7)
  # links_knn <- knn2nb(k_neighbor_list)
  # 1. Create the data frame of connected indices
  tri <- tri2nb(coords)

  Edges_tri <- expp::neighborsDataFrame(tri)

  links_matrix_expp <- as.matrix(Edges_tri[, c("id", "id_neigh")])

  links_matrix_expp
}

prepare_knn_edges <- function(x) {
  coords <- st_centroid(st_geometry(x))

  k_neighbor_list <- knearneigh(coords, k = 7)
  links_knn <- knn2nb(k_neighbor_list)

  links_matrix_knn <- as_edgelist(links_knn)

  links_matrix_knn
}
