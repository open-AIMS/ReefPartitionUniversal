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

test_that("Numbers of clusters allocated across 2 even habitats is around equal", {
  expect_equal(
    mean(table(clustered_points[clustered_points$habitat == 1, ]$site_id)) /
      mean(table(clustered_points[clustered_points$habitat == 2, ]$site_id)),
    1,
    tolerance = 0.25
  )
})
