# testing MST calculations on pregenerated random data

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

extracted_points <- extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  output_epsg = 4326
)
extracted_points <- extracted_points[!is.na(extracted_points$depth), ]

# Perform formatting that is conducted in cluster_reef_pixels
extracted_points <- dplyr::distinct(extracted_points)
extracted_points$X_standard <- scale(extracted_points$X)
extracted_points$Y_standard <- scale(extracted_points$Y)
extracted_points$depth_standard <- scale(extracted_points$depth)

habitat_points <- extracted_points[extracted_points$habitat == 1, ]

# Test the outputs of prepare_mst function
mst <- prepare_mst(sf::st_transform(habitat_points, crs = 3857))

test_that("output is igraph format", {
  expect_true(inherits(mst, "igraph"))
})

test_that("all points accounted", {
  expect_equal(length(mst), nrow(habitat_points))
})

test_that("mean weights are the same as precalculated", {
  # Recomputed after fixing neighborsDataFrame()'s row-misalignment bug
  # (see NEWS.md) -- the old value (0.39) was calculated against edge costs
  # that had been paired with the wrong edges.
  expect_equal(mean(igraph::E(mst)$weight), 0.51, tolerance = 0.1)
})

test_that("mean edge lengths are the same as precalculated", {
  # Recomputed after fixing neighborsDataFrame()'s row-misalignment bug
  # (see NEWS.md) -- the resulting MST's edge set shifted since it's
  # selected using the (now-correct) edge weights.
  expect_equal(
    as.numeric(mean(igraph::E(mst)$length)),
    53000,
    tolerance = 0.1
  )
})

test_that("neighborsDataFrame preserves natural per-node order (not sorted by id)", {
  # Regression test for a bug where neighborsDataFrame()'s two merge() calls
  # silently reordered rows relative to nb's natural per-node order, causing
  # prepare_mst() to zip spdep::nbcosts()'s cost vector (which iterates in
  # that same natural order, with no sorting) against the wrong edges. A
  # hand-built nb object with a deliberately unsorted region.id and multiple
  # neighbors per node distinguishes "natural order" from any id-sorted
  # order an implementation might silently produce instead.
  nb <- list(c(2L, 3L), c(1L, 3L), c(1L, 2L))
  attr(nb, "region.id") <- c("c", "a", "b")
  class(nb) <- "nb"

  expected_id <- c("c", "c", "a", "a", "b", "b")
  expected_id_neigh <- c("a", "b", "c", "b", "c", "a")

  result <- ReefPartitionUniversal:::neighborsDataFrame(nb)

  expect_identical(result$id, expected_id)
  expect_identical(result$id_neigh, expected_id_neigh)
})

test_that("prepare_mst's MST edge weights are internally consistent with an independent recomputation", {
  # Integration-level counterpart to the neighborsDataFrame() unit test
  # above: independently reproduces prepare_mst()'s full weight pipeline
  # (triangulate -> per-edge cost/length -> z-score -> combine) using the
  # fixed neighborsDataFrame(), and verifies every edge that actually
  # survives into prepare_mst()'s returned MST has a weight matching this
  # independent recomputation *for that same (i, j) pair* -- i.e. that costs
  # are assigned to the correct edges end-to-end, not just that
  # neighborsDataFrame() looks right in isolation.
  mst_alpha_check <- 0.7
  habitat_points_3857 <- sf::st_transform(habitat_points, crs = 3857)

  coords <- sf::st_centroid(sf::st_geometry(habitat_points_3857))
  tri <- spdep::tri2nb(coords)
  costs_tri <- unlist(spdep::nbcosts(
    tri,
    data = habitat_points_3857[, "depth_standard", drop = TRUE],
    method = "manhattan"
  ))
  edges_tri <- ReefPartitionUniversal:::neighborsDataFrame(tri)
  from <- as.numeric(edges_tri$id)
  to <- as.numeric(edges_tri$id_neigh)

  coords_mat <- sf::st_coordinates(coords)
  raw_length <- sqrt(
    (coords_mat[from, 1] - coords_mat[to, 1])^2 +
      (coords_mat[from, 2] - coords_mat[to, 2])^2
  )
  length_scaled <- as.numeric(scale(raw_length))
  weight_scaled <- as.numeric(scale(costs_tri))
  expected_weight <- sqrt(
    (length_scaled^2) * (1 - mst_alpha_check) + (weight_scaled^2) * mst_alpha_check
  )
  names(expected_weight) <- paste(pmin(from, to), pmax(from, to))

  mst_check <- prepare_mst(habitat_points_3857, mst_alpha = mst_alpha_check)
  mst_edges <- igraph::as_edgelist(mst_check)
  actual_keys <- paste(
    pmin(mst_edges[, 1], mst_edges[, 2]),
    pmax(mst_edges[, 1], mst_edges[, 2])
  )

  expect_equal(
    unname(igraph::E(mst_check)$weight),
    unname(expected_weight[actual_keys]),
    tolerance = 1e-8
  )
})
