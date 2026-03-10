# testing H3 cell extraction

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

# 1. Test the outputs of extract_point_cells()
extracted_points <- extract_point_cells(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  hex_resolution = 4,
  output_epsg = 4326
)

test_that("number of points extracted", {
  expect_equal(nrow(extracted_points), 748)
})

test_that("extracted only requested habitats", {
  expect_true(all(unique(extracted_points$habitat) %in% habitat_categories))
})

test_that("extracted habitats roughly equal", {
  expect_equal(
    unname(table(extracted_points$habitat)[1]) /
      sum(table(extracted_points$habitat)),
    0.5,
    tolerance = 0.1
  )
})

test_that("extracted depth mean around 10", {
  expect_equal(
    mean(extracted_points$depth, na.rm = TRUE),
    10,
    tolerance = 0.1
  )
})

test_that("extracted depth column contains no NAs (interpolation = TRUE)", {
  expect_false(any(is.na(extracted_points$depth)))
})

test_that("output data frame contains all default required columns for later operations", {
  expect_true(all(
    c("h3_index", "habitat", "depth", "X", "Y", "geometry") %in%
      names(extracted_points)
  ))
})

test_that("goemetries are points", {
  expect_true(inherits(sf::st_geometry(extracted_points), "sfc_POINT"))
})
