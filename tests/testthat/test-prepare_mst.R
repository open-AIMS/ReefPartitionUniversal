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

test_that("all points accounted", {
  expect_equal(length(mst), nrow(habitat_points))
})

test_that("mean weights are the same as precalculated", {
  expect_equal(mean(igraph::E(mst)$weight), 0.27, tolerance = 0.01)
})

test_that("mean edge lengths are the same as precalculated", {
  expect_equal(
    as.numeric(mean(igraph::E(mst)$length)),
    44508.31,
    tolerance = 0.01
  )
})
