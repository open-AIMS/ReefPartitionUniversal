# testing cluster_reef_pixels function

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

extracted_points <- extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  output_epsg = 3857
)
extracted_points$UNIQUE_ID <- "test_reef"

desired_points <- 50
res <- 0.25 * 111320

clustered_points <- cluster_reef_points(
  extracted_points,
  clustering_function_args = list(
    site_size = 50 * (res * res),
    point_area = res * res
  )
)

clustered_points <- sf::st_transform(clustered_points, crs = 4326)
clustered_points$X <- sf::st_coordinates(clustered_points)[, 1]
clustered_points$Y <- sf::st_coordinates(clustered_points)[, 2]

clust_poly <- pixels_to_polygons(
  clustered_points,
  UNIQUE_ID,
  habitat,
  depth,
  pixel_size = 0.25
)

test_that("same number of unique sites", {
  expect_equal(
    length(unique(clust_poly$site_id)),
    length(unique(clustered_points$site_id))
  )
})

test_that("
    roughly correct spatial area as points (only expect roughly equal due
    to CRS differences and inexact calculation of pixel resolution)
    ", {
  expect_equal(
    as.numeric(sum(sf::st_area(clust_poly))),
    nrow(clustered_points) * (res * res),
    tolerance = 0.25
  )
})
