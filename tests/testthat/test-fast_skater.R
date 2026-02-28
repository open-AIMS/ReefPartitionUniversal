# testing fast skater functioning using pregenerated random data

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

extracted_points <- extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  output_epsg = 3112
)
extracted_points <- extracted_points[!is.na(extracted_points$depth), ]

# Perform formatting that is conducted in cluster_reef_pixels
extracted_points <- dplyr::distinct(extracted_points)
extracted_points$X_standard <- scale(extracted_points$X)
extracted_points$Y_standard <- scale(extracted_points$Y)
extracted_points$depth_standard <- scale(extracted_points$depth)

habitat_points <- extracted_points[extracted_points$habitat == 1, ]
habitat_points$UNIQUE_ID <- "test_reef"

# With 250 * 250 desired site size and 553 points and rough cell resolution of 2500m, the desired number of clusters is:
desired_clusters <- (15000 * 15000 * nrow(habitat_points)) / (100000 * 100000)

# Test outputs of reef_skater_fast function when used with pregenerated data
clustered_points <- reef_skater_fast(
  habitat_points,
  site_size = 100000 * 100000,
  cell_resolution = 15000 * 15000 # Set desired number of points per cluster to 100
)

# Test number of clusters is roughly equal
test_that("number of clusters", {
  expect_equal(
    length(unique(clustered_points$site_id)),
    desired_clusters,
    tolerance = 0.3
  )
})
