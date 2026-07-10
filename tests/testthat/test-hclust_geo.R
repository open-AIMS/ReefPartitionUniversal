# testing standard (unconstrained) hclust_geo functioning using pregenerated random data

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
habitat_points$UNIQUE_ID <- "test_reef"

# Test outputs of hclust_geo function when used with pregenerated data
clustered_points <- hclust_geo(
  habitat_points,
  n_points = 100 # Set desired number of points per cluster to 100
)

# Test number of clusters is roughly equal
test_that("number of clusters", {
  expect_equal(
    length(unique(clustered_points$site_id)),
    nrow(clustered_points) / 100,
    tolerance = 0.25
  )
})

# Test mean number of points per cluster is around requested (100)
test_that("points per cluster", {
  expect_equal(
    as.numeric(mean(table(clustered_points$site_id))),
    100,
    tolerance = 0.25
  )
})

test_that("ensure all required columns are present in output", {
  expect_true(all(
    c(
      "habitat",
      "X_standard",
      "Y_standard",
      "depth_standard",
      "UNIQUE_ID",
      "site_id",
      "geometry"
    ) %in%
      names(clustered_points)
  ))
})

test_that("ensure geometries are points", {
  expect_true(inherits(sf::st_geometry(clustered_points), "sfc_POINT"))
})

test_that("clustering is unaffected by depth values", {
  scrambled_points <- habitat_points
  scrambled_points$depth_standard <- sample(scrambled_points$depth_standard)

  scrambled_clustered <- hclust_geo(
    scrambled_points,
    n_points = 100
  )

  expect_identical(
    as.character(clustered_points$site_id),
    as.character(scrambled_clustered$site_id)
  )
})
