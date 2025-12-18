prepare_mst <- function(x, additional_variable_cols = c("depth_standard"), alpha = 0.5, hex_resolution = 12) {
    add_var_weight <- alpha
    geo_weight <- 1 - alpha
    coords <- sf::st_centroid(st_geometry(x))

  # Triangulate edges between pixel points
  tri <- spdep::tri2nb(coords)
  Costs_tri <- spdep::nbcosts(tri, data = x[, additional_variable_cols, drop = TRUE], method = "manhattan")
  Costs_tri <- unlist(Costs_tri)

  Edges_tri <- expp::neighborsDataFrame(tri)

  Edges_tri2 <- data.frame(from = as.numeric(Edges_tri$id), to = as.numeric(Edges_tri$id_neigh), weights = Costs_tri)
  Network_withEdgesTri <- sfnetwork::sfnetwork(x, Edges_tri2[, c(1, 2)], directed = FALSE)
  E(Network_withEdgesTri)$weight <- Costs_tri

  Network_withEdgesTri <- Network_withEdgesTri %>%
    tidygraph::activate("edges") %>%
    mutate(length = sfnetworks::edge_length())
  Length_scaled <- scale(as.numeric(E(Network_withEdgesTri)$length))
  Weight_scaled <- scale(as.numeric(E(Network_withEdgesTri)$weight))

  Eucliden_weight_old <- sqrt((Length_scaled^2) * geo_weight + (Weight_scaled^2) * add_var_weight)
  E(Network_withEdgesTri)$weight <- Eucliden_weight_old

  # Triangulation
  mst_tri <- igraph::mst(Network_withEdgesTri, weights = E(Network_withEdgesTri)$weight) #+Length_scaled)

  mst_tri
}

prepare_tri_edges <- function(x) {
  coords <- sf::st_centroid(st_geometry(x))

  # k_neighbor_list <- knearneigh(coords, k = 7)
  # links_knn <- knn2nb(k_neighbor_list)
  # 1. Create the data frame of connected indices
  tri <- spdep::tri2nb(coords)

  Edges_tri <- expp::neighborsDataFrame(tri)

  links_matrix_expp <- as.matrix(Edges_tri[, c("id", "id_neigh")])

  links_matrix_expp
}

prepare_knn_edges <- function(x) {
  coords <- sf::st_centroid(st_geometry(x))

  k_neighbor_list <- spdep::knearneigh(coords, k = 7)
  links_knn <- spdep::knn2nb(k_neighbor_list)

  links_matrix_knn <- igraph::as_edgelist(links_knn)

  links_matrix_knn
}
