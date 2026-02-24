#' Extract pixel data from selected habitats over a target reef.
#'
#' @description Extract data from the centroids of pixels that overlap the target `reef_polyon`.
#'   Extraction is performed for two raster layers, one `habitat_raster` must contain habitat
#'   categories that form the basis of the pixel centroids, the other `add_var_raster` is
#'   additional desired data for clustering at later points in the workflow. Extrction method
#'   uses habitat_raster pixels, meaning points have a minimum distance apart of res(habitat_raster) / 2.
#'
#' @param reef_polygon sf_object. sf object containing the target reef polygons for data
#'   coverage.
#' @param habitat_raster SpatRaster. Terra raster object containing categorical values for
#'   habitats covering the target reef area.
#' @param add_var_raster SpatRaster. Terra raster object containing an additional variable
#'   to extract for all selected habitat pixels covering the target reef area. Can be continuous
#'   or categorical, however the raster will be resampled and the method should be changed for
#'   categorical data.
#' @param habitat_categories character. Vector containing the habitat categories to select from
#'   `habitat_raster`.
#' @param additional_variable_name character. Name to assign the extracted data from
#'   `add_var_raster`. Default = "depth".
#' @param output_epsg integer. EPSG code used for outputting pixels and extracted data.
#'   Default = 3112.
#' @param interpolation bool. Option to interpolate missing values after extracting
#'   data from `add_var_raster` using nearest neighbour interpolation.
#'
#' @return data.frame containing points for pixels from `habitat_raster` covering `reef_polygon`
#'   for selected habitats in `habitat_categories`, alongside extracted data from `add_var_raster`.
#'
#' @importFrom terra %in%
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#'
#' @export
#'
extract_point_pixels <- function(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  additional_variable_name = "depth",
  output_epsg = 3112,
  interpolation = TRUE
) {
  # Perform input data checks before proceeding with computations
  input_check(reef_polygon, habitat_raster, add_var_raster, habitat_categories)

  # Crop the input raster layers for faster extraction
  reef_polygon_terra <- terra::vect(reef_polygon)
  habitat_cropped <- terra::crop(habitat_raster, reef_polygon_terra)
  add_var_cropped <- terra::crop(
    add_var_raster,
    reef_polygon_terra,
    mask = TRUE
  )

  names(habitat_cropped)[1] <- "categorical_habitat"

  # Extract cell points from the habitat raster object
  pts <- terra::extract(
    terra::mask(habitat_cropped, reef_polygon),
    reef_polygon,
    xy = TRUE,
    cells = TRUE,
    ID = FALSE
  ) %>%
    na.omit()
  pts <- pts[pts$categorical_habitat %in% habitat_categories, ] # filter habitats
  pixel_points <- st_as_sf(pts, coords = c("x", "y"), crs = reef_crs)

  additional_var_values <- terra::extract(
    add_var_cropped,
    pixel_points,
    df = TRUE
  )
  colnames(additional_var_values)[2] <- additional_variable_name
  pixel_points[, additional_variable_name] <- additional_var_values[,
    additional_variable_name
  ]

  # Clean up pixels and extracted data
  # Transform Points to match the datas' CRS (by default h3 points do not have a CRS)
  pixel_points <- sf::st_transform(pixel_points, reef_crs)

  pixel_points <- pixel_points %>%
    dplyr::filter(!is.na(sf::st_dimension(.))) %>% # Remove NA dimensions
    sf::st_make_valid()

  hab_pts <- pixel_points %>%
    rename(geomorph = "categorical_habitat") %>%
    sf::st_transform(output_epsg) %>% # project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
    dplyr::bind_cols(., as.data.frame(sf::st_coordinates(.))) %>%
    dplyr::filter(
      !is.na(geomorph),
      if (!is.null(habitat_categories)) {
        geomorph %in% habitat_categories
      } else {
        TRUE
      }
    ) %>% # Handle NULL geozone_list
    rename(habitat = geomorph)

  if (interpolation) {
    hab_pts <- fill_na_nearest(hab_pts, additional_variable_name)
  }

  return(hab_pts)
}

#' Internal helper function that fills NA values from columns with values from
#' the nearest neighbouring points.
fill_na_nearest <- function(pixel_data, columns) {
  for (col in columns) {
    if (any(is.na(pixel_data[[col]]))) {
      has_value <- which(!is.na(pixel_data[[col]]))
      has_na <- which(is.na(pixel_data[[col]]))

      if (length(has_value) == 0) {
        next
      }

      coords_with_value <- sf::st_coordinates(sf::st_centroid(pixel_data[
        has_value,
      ]))
      coords_with_na <- sf::st_coordinates(sf::st_centroid(pixel_data[
        has_na,
      ]))

      nearest_idx <- apply(coords_with_na, 1, function(na_coord) {
        # Calculate Euclidean distance without transpose
        distances <- sqrt(
          (coords_with_value[, 1] - na_coord[1])^2 +
            (coords_with_value[, 2] - na_coord[2])^2
        )
        has_value[which.min(distances)]
      })

      pixel_data[[col]][has_na] <- pixel_data[[col]][nearest_idx]
    }
  }

  return(pixel_data)
}
