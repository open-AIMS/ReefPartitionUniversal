# testing cluster_reef_pixels function

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
extracted_points$UNIQUE_ID <- "test_reef"
extracted_points <- sf::st_transform(extracted_points, crs = 3857)

clustered_points <- cluster_reef_pixels(
  extracted_points,
  clustering_function_args = list(n_pixels = 100)
)
