# Test input check function

# Define shared testing data that is valid
source(test_path("setup_test_data.R"))

#### 1. Test input check functions for correct output with valid data
test_that("input_check for valid data - no error", {
  expect_equal(
    input_check(
      reef_polygon,
      habitat_raster,
      add_var_raster,
      habitat_categories
    ),
    NULL
  )
})


#### 2. Test CRS alignment
# 2.1. Test CRS error when habitat_raster is non-matching
test_that("habitat raster crs check", {
  new_habitat_raster <- habitat_raster
  crs(new_habitat_raster) <- "epsg:3112"
  expect_error(
    input_check(
      reef_polygon,
      new_habitat_raster,
      add_var_raster,
      habitat_categories
    ),
    class = "crs_mismatch"
  )
})

# 2.2. Test CRS error when add_var_raster is non-matching
test_that("additional variable raster crs check", {
  new_add_var_raster <- add_var_raster
  crs(new_add_var_raster) <- "epsg:3112"
  expect_error(
    input_check(
      reef_polygon,
      habitat_raster,
      new_add_var_raster,
      habitat_categories
    ),
    class = "crs_mismatch"
  )
})

#### 3. Test extent alignment
# 3.1. Test reef extent alignment when habitat_raster does not overlap reef_polygon
x_coords <- seq(100, 110, 0.25)
y_coords <- seq(10, 20, 0.25)
pixel_size <- 0.25

test_that("habitat raster reef intersection", {
  new_habitat_raster <- terra::rast(
    extent = terra::ext(
      min(x_coords) - pixel_size / 2,
      max(x_coords) + pixel_size / 2,
      min(y_coords) - pixel_size / 2,
      max(y_coords) + pixel_size / 2
    ),
    resolution = pixel_size,
    crs = "epsg:4326"
  )
  terra::values(new_habitat_raster) <- sample(
    c(NA, 1, 2),
    length(terra::values(new_habitat_raster)),
    replace = TRUE
  )

  expect_error(
    input_check(
      reef_polygon,
      new_habitat_raster,
      add_var_raster,
      habitat_categories
    ),
    class = "no_intersection"
  )
})

# 3.2. Test reef extent alignment when add_var_raster does not overlap reef_polygon
test_that("additional variable raster reef intersection", {
  new_add_var_raster <- terra::rast(
    extent = terra::ext(
      min(x_coords) - pixel_size / 2,
      max(x_coords) + pixel_size / 2,
      min(y_coords) - pixel_size / 2,
      max(y_coords) + pixel_size / 2
    ),
    resolution = pixel_size,
    crs = "epsg:4326"
  )
  terra::values(new_add_var_raster) <- runif(
    length(terra::values(new_add_var_raster)),
    0,
    20
  )

  expect_error(
    input_check(
      reef_polygon,
      habitat_raster,
      new_add_var_raster,
      habitat_categories
    ),
    class = "no_intersection"
  )
})

#### 4. Test valid data overlapping reef_polygon
# 4.1. Test when habitat_raster does not contain valid data for the target reef
test_that("invalid habitat raster values", {
  invalid_habitat_raster <- habitat_raster
  values(invalid_habitat_raster) <- NA

  expect_error(
    input_check(
      reef_polygon,
      invalid_habitat_raster,
      add_var_raster,
      habitat_categories
    ),
    class = "invalid_data"
  )
})

# 4.1. Test when add_var_raster does not contain valid data for the target reef
test_that("invalid additional variable raster values", {
  invalid_add_var_raster <- add_var_raster
  values(invalid_add_var_raster) <- NA

  expect_error(
    input_check(
      reef_polygon,
      habitat_raster,
      invalid_add_var_raster,
      habitat_categories
    ),
    class = "invalid_data"
  )
})
