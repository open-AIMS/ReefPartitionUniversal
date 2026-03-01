# Test the vector_to_raster

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

habitat_vector <- sf::st_as_sf(terra::as.polygons(habitat_raster)) %>%
  dplyr::rename(habitat = lyr.1)

habitat_raster_gen <- vector_to_raster(
  habitat_vector,
  "habitat",
  output_file = NA,
  raster_template = generate_raster_template(habitat_vector, 0.25)
)

test_that("the generated habitat raster values are identical to the original", {
  expect_identical(
    as.double(terra::values(habitat_raster_gen)),
    as.double(terra::values(habitat_raster))
  )
})

test_that("generated habitat raster has same extent as original data", {
  expect_identical(sf::st_bbox(habitat_raster_gen), sf::st_bbox(habitat_raster))
})

test_that("generated habitat raster has same resolution as original data", {
  expect_identical(terra::res(habitat_raster_gen), terra::res(habitat_raster))
})

test_that("generated habitat raster has same CRS as original data", {
  expect_identical(terra::crs(habitat_raster_gen), terra::crs(habitat_raster))
})
