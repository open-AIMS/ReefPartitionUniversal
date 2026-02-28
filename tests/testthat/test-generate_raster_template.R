# Test the generate_raster_template function

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

habitat_vector <- sf::st_as_sf(terra::as.polygons(habitat_raster)) %>%
  dplyr::rename(habitat = lyr.1)

raster_template <- generate_raster_template(habitat_vector, 0.25)

test_that("raster template has same extent as original data", {
  expect_equal(sf::st_bbox(raster_template), sf::st_bbox(habitat_raster))
})

test_that("raster template has same resolution as original data", {
  expect_equal(terra::res(raster_template), terra::res(habitat_raster))
})

test_that("raster template has same CRS as original data", {
  expect_equal(terra::crs(raster_template), terra::crs(habitat_raster))
})
