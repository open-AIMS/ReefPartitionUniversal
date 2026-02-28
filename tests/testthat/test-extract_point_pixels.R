# testing pixel extraction

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

# 1. Test the outputs of extract_pixel_points()
extracted_points <- extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  output_epsg = 4326
)

test_that("number of points extracted", {
  expect_equal(nrow(extracted_points), 1725)
})

test_that("extracted only requested habitats", {
  expect_true(all(unique(extracted_points$habitat) %in% habitat_categories))
})

test_that("extracted habitats roughly equal", {
  expect_equal(
    unname(table(extracted_points$habitat)[1]) /
      sum(table(extracted_points$habitat)),
    0.5,
    tolerance = 0.01
  )
})

test_that("extracted depth mean around 10", {
  expect_equal(
    mean(extracted_points$depth, na.rm = TRUE),
    10,
    tolerance = 0.01
  )
})
