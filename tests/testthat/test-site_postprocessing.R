# testing site_postprocessing function

# Define shared testing data that is valid
source(test_path("setup_test_inputs.R"))

extracted_points <- extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  output_epsg = 3857
)
extracted_points$UNIQUE_ID <- "test_reef"

desired_points <- 50
res <- 0.25 * 111320

clustered_points <- cluster_reef_points(
  extracted_points,
  clustering_function_args = list(
    site_size = 50 * (res * res),
    point_area = res * res
  )
)

clustered_points <- sf::st_transform(clustered_points, crs = 4326)
clustered_points$X <- sf::st_coordinates(clustered_points)[, 1]
clustered_points$Y <- sf::st_coordinates(clustered_points)[, 2]

clust_poly <- pixels_to_polygons(
  clustered_points,
  UNIQUE_ID,
  habitat,
  depth,
  pixel_size = 0.25
)

max_distance <- 15000
min_site_area <- 2 * (res * res)

post_process <- site_postprocessing(
  clust_poly,
  min_site_area = min_site_area,
  max_distance = max_distance
)

test_that("
    Ensure post processing didn't result in any multipolygon subcomponents with
    distances > max_distance from their largest subcomponent.
    ", {
  poor_rows <- c()

  for (r in seq_along(nrow(post_process))) {
    polygon = post_process[r, ]
    polygon

    seperate_polygons <- data.frame(
      geometry = rep(NA, length(polygon$geometry[[1]])),
      area = rep(NA, length(polygon$geometry[[1]]))
    )

    for (lists in 1:length(polygon$geometry[[1]])) {
      individual_poly <- sf::st_sfc(
        sf::st_polygon(polygon$geometry[[1]][[
          lists
        ]]),
        crs = site_polygons_crs
      ) %>%
        sf::st_set_crs(site_polygons_crs)
      seperate_polygons$geometry[
        lists
      ] <- individual_poly
      seperate_polygons$area[lists] <- sf::st_area(individual_poly)
    }

    distances <- sf::st_distance(sf::st_as_sfc(
      seperate_polygons$geometry,
      crs = site_polygon_crs
    ))
    dist_to_largest <- distances[
      which(seperate_polygons$area == max(seperate_polygons$area)),
    ]

    if (any(as.numeric(dist_to_largest) > max_distance)) {
      poor_rows <- c(poor_rows, r)
    }
  }

  expect_equal(poor_rows, NULL)
})

test_that("Ensure that post processing has not resulted in any site multipolygons that are smaller than min_site_area.", {
  expect_false(any(as.numeric(sf::st_area(post_process)) < min_site_area))
})
