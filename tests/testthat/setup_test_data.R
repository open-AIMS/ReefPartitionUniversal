set.seed(123)

habitat_categories <- c(1, 2)
x_coords <- seq(150, 160, 0.25)
y_coords <- seq(10, 20, 0.25)
pixel_size <- 0.25

# Create generic raster with randomly allocated habitats
habitat_raster <- terra::rast(
  extent = terra::ext(
    min(x_coords) - pixel_size / 2,
    max(x_coords) + pixel_size / 2,
    min(y_coords) - pixel_size / 2,
    max(y_coords) + pixel_size / 2
  ),
  resolution = pixel_size,
  crs = "epsg:4326"
)
terra::values(habitat_raster) <- sample(
  c(NA, 1, 2),
  length(terra::values(habitat_raster)),
  replace = TRUE
)

# Create generic raster with randomly drawn additional variable values
add_var_raster <- habitat_raster
terra::values(add_var_raster) <- runif(
  length(terra::values(add_var_raster)),
  0,
  20
)

# Create reef polygon as a bounding box with the same extent as the rasters
reef_polygon <- sf::st_as_sfc(sf::st_bbox(add_var_raster))
