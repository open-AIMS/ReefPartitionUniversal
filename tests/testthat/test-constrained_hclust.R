# testing constrained hclust functioning using pregenerated random data

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

extracted_points <- extract_pixel_points(
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
habitat_points$UNIQUE_ID <- "test_reef"

mst <- prepare_mst(sf::st_transform(habitat_points, crs = 3857))

# Test outputs of constrained_hclust function when used with pregenerated data
clustered_points <- constrained_hclust(
  habitat_points,
  igraph::as_edgelist(mst),
  n_pixels = 100 # Set desired number of points per cluster to 100
)

# Test number of clusters is roughly equal
test_that("number of clusters", {
  expect_equal(
    length(unique(clustered_points$site_id)),
    nrow(clustered_points) / 100,
    tolerance = 1
  )
})

# Test mean number of points per cluster is around requested (100)
test_that("points per cluster", {
  expect_equal(
    as.numeric(mean(table(clustered_points$site_id))),
    100,
    tolerance = 10
  )
})
