#' Create a minimum spanning tree from geographic coordinates of pixels and extracted
#' data.
#' 
#' @description Take a dataframe of pixels `pixels` and create a minimum spanning tree
#'   between pixel coordinates, using edge costs that are a combination of geographic
#'   distances and distances between pixels by `additional_variable_cols` values.
#'   For additional details on minimum spanning tree creation see igraph::mst().
#' 
#' @param pixels sf data.frame. Holds values for pixel geometries and 
#'   `additional_variable_cols`
#' @param additional_variable_cols character vector. Names of the columns to extract
#'   additional (non-geometric) data from for cost weighting.
#' @param alpha float numeric. Weighting applied to `additional_variable_cols`
#'   distance in edge cost weighting when combining with geographic distances.
#'   (1 - alpha) weight is applied to the geographic distance. Default = 0.5
#'   (same weight for `additional_variable_cols` and geographic distances).
#' @param hex_resolution integer. H3 hex resolution of pixels. Default = 12.
#' 
#' @return igraph::mst Minimum spanning tree object.
#' 
#' @export
#' 
prepare_mst <- function(pixels, additional_variable_cols = c("depth_standard"), alpha = 0.5, hex_resolution = 12) {
    add_var_weight <- alpha
    geo_weight <- 1 - alpha
    coords <- sf::st_centroid(st_geometry(pixels))

  # Triangulate edges between pixel points
  tri <- spdep::tri2nb(coords)
  Costs_tri <- spdep::nbcosts(tri, data = pixels[, additional_variable_cols, drop = TRUE], method = "manhattan")
  Costs_tri <- unlist(Costs_tri)

  Edges_tri <- expp::neighborsDataFrame(tri)

  Edges_tri2 <- data.frame(from = as.numeric(Edges_tri$id), to = as.numeric(Edges_tri$id_neigh), weights = Costs_tri)
  Network_withEdgesTri <- sfnetwork::sfnetwork(pixels, Edges_tri2[, c(1, 2)], directed = FALSE)
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

#' Create triangulated edges from geographic coordinates of pixels.
#' 
#' @description Take a dataframe of pixels `pixels` and triangulate the vertices.
#'   Then extract the edges between pixels for clustering inputs.
#' 
#' @param pixels sf data.frame. Holds values for pixel geometries.
#' 
#' @return Matrix containing trianguated edge values between pixels.
#' 
#' @export
#' 
prepare_tri_edges <- function(pixels) {
  coords <- sf::st_centroid(st_geometry(pixels))

  tri <- spdep::tri2nb(coords)

  Edges_tri <- expp::neighborsDataFrame(tri)

  links_matrix_expp <- as.matrix(Edges_tri[, c("id", "id_neigh")])

  links_matrix_expp
}

#' Create k-nearest-neighbour edges for clustering inputs.
#' 
#' @description Take a dataframe of pixels `pixels` and create k-nearest-neighbour
#'   graphs between pixels based on geographic distances.
#' 
#' @param pixels sf data.frame. Holds values for pixel geometries.
#' 
#' @param k integer. K-nearest-neighbours
#' 
#' @return Matrix containing k-n-n edge values between pixels.
#' 
#' @export
#' 
prepare_knn_edges <- function(pixels, k = 7) {
  coords <- sf::st_centroid(st_geometry(pixels))

  k_neighbor_list <- spdep::knearneigh(coords, k = k)
  links_knn <- spdep::knn2nb(k_neighbor_list)

  links_matrix_knn <- igraph::as_edgelist(links_knn)

  links_matrix_knn
}
