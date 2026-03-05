# Check the function that verifies CRS units
points <- rbind(
  c(150, 10),
  c(160, 10),
  c(155, 30),
  c(150, 10)
)
poly <- sf::st_polygon(list(points))

sf_data_degree <- sf::st_as_sfc(sf::st_as_text(poly), crs = 4326)
sf_data_metre <- sf::st_transform(sf_data_degree, crs = 3857)

test_that("function can detect degree unit", {
  expect_true(check_unit(sf_data_degree, "degree"))
})

test_that("function can detect metre unit", {
  expect_true(check_unit(sf_data_metre, "metre"))
})

test_that("function can detect incorrect unit", {
  expect_false(check_unit(sf_data_degree, "metre"))
})
