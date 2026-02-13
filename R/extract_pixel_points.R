#' Extract pixel data from selected habitats over a target reef.
#'
#' @description Extract data from the centroids of pixels that overlap the target `reef_polyon`.
#'   Extraction is performed for two raster layers, one `habitat_raster` must contain habitat
#'   categories that form the basis of the pixel centroids, the other `add_var_raster` is
#'   additional desired data for clustering at later points in the workflow.
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
#' @param hex_resolution integer. Selected H3 resolution of hexagons for pixel representation.
#'   Default = 12.
#' @param unit character. Unit used by H3 functions to calculate the area of hexagons.
#'   Default = "km2".
#' @param additional_variable_name character. Name to assign the extracted data from
#'   `add_var_raster`. Default = "depth".
#' @param output_epsg integer. EPSG code used for outputting pixels and extracted data.
#'   Default = 3112.
#' @param resample_method Method used to resample `add_var_raster` before extracting pixel
#'   data. Default = "bilinear", method should be changed for categorical data.
#'   See [terra::disagg()] for more details.
#'
#' @return data.frame containing `habitat_raster` pixels covering `reef_polygon`
#'   for selected habitats in `habitat_categories`, alongside extracted data from `add_var_raster`.
#'
#' @importFrom terra %in%
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#'
#' @export
#'
extract_pixel_points <- function(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  hex_resolution = 12,
  unit = "km2",
  additional_variable_name = "depth",
  output_epsg = 3112,
  resample_method = "bilinear"
) {
  # Perform input data checks before proceeding with computations

  # Ensure that the reef_polygon and categorical habitat raster have the same CRS
  reef_crs <- sf::st_crs(reef_polygon)
  if (sf::st_crs(habitat_raster) != reef_crs) {
    stop(
      "Ensure st_crs(habitat_raster) == st_crs(reef_polygon) before proceeding."
    )
  }

  # Ensure that the habitat raster and additional variable raster have the same CRS
  if (sf::st_crs(habitat_raster) != sf::st_crs(add_var_raster)) {
    stop(
      "Ensure st_crs(habitat_raster) == st_crs(add_var_raster) before proceeding."
    )
  }

  # Check if reef_polygon intersects with the habitat_raster
  hab_raster_bbox <- sf::st_bbox(habitat_raster)
  habitat_intersects <- sf::st_intersects(
    sf::st_as_sfc(hab_raster_bbox),
    reef_polygon,
    sparse = FALSE
  )

  if (!any(habitat_intersects)) {
    stop(
      "
            Ensure that reef_polygon intersects with habitat_raster.
            `st_intersects(st_as_sfc(st_bbox(habitat_raster))), reef_polygon, sparse = FALSE)`
        "
    )
  }

  # Check if reef_polygon intersects with the habitat_raster
  add_var_raster_bbox <- sf::st_bbox(add_var_raster)
  add_var_intersects <- sf::st_intersects(
    sf::st_as_sfc(add_var_raster_bbox),
    reef_polygon,
    sparse = FALSE
  )

  if (!any(add_var_intersects)) {
    stop(
      "
            Ensure that reef_polygon intersects with add_var_raster.
            `st_intersects(st_as_sfc(st_bbox(add_var_raster))), reef_polygon, sparse = FALSE)`
        "
    )
  }

  # Crop the input raster layers for faster extraction
  reef_polygon_terra <- terra::vect(reef_polygon)
  habitat_cropped <- terra::crop(habitat_raster, reef_polygon_terra)
  add_var_cropped <- terra::crop(
    add_var_raster,
    reef_polygon_terra,
    mask = TRUE
  )

  if (terra::ncell(habitat_cropped) == 0) {
    stop(
      "Cropping resulted in empty raster - check if reef_polygon overlaps with valid raster data."
    )
  }

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
  raster_res <- terra::res(habitat_cropped)[1]
  half_res <- raster_res / 2

  # Convert cell points into cell grid squares using the resolution of the raster
  # to emulate the result obtained using a stars object
  squares_list <- lapply(1:nrow(pts), function(i) {
    x <- pts$x[i]
    y <- pts$y[i]
    st_polygon(list(matrix(
      c(
        x - half_res,
        y + half_res, # Top Left
        x + half_res,
        y + half_res, # Top Right
        x + half_res,
        y - half_res, # Bottom Right
        x - half_res,
        y - half_res, # Bottom Left
        x - half_res,
        y + half_res # Close the loop (Back to Top Left)
      ),
      ncol = 2,
      byrow = TRUE
    )))
  })

  # Collate grid square sf polygons and convert to h3 indices
  point_cells <- sf::st_sfc(squares_list, crs = reef_crs) %>%
    sf::st_sf(data = pts[, !names(pts) %in% c("x", "y")])
  hexid <- h3::geo_to_h3(point_cells, res = 12)

  hexid <- unique(hexid) # Remove pixels with the same coordinates
  pixel_points <- h3::h3_to_geo_sf(hexid) # Get the centers of the given H3 indexes as sf object.

  if (length(hexid) < 2) {
    stop("Less than 2 pixels identified from inputs.")
  }

  # Extract values from the additional variable raster layer and attach them to points
  add_var_resampled <- terra::disagg(
    add_var_cropped,
    fact = 5,
    method = resample_method
  )

  additional_var_values <- terra::extract(
    add_var_resampled,
    pixel_points,
    df = TRUE
  )
  colnames(additional_var_values)[2] <- additional_variable_name
  pixel_points[, additional_variable_name] <- additional_var_values[,
    additional_variable_name
  ]

  # Clean up pixels and extracted data
  # Transform Pixels to match the datas' CRS (by default h3 points do not have a CRS)
  pixel_points <- sf::st_transform(pixel_points, reef_crs)

  pixel_points <- pixel_points %>%
    dplyr::filter(!is.na(sf::st_dimension(.))) %>% # Remove NA dimensions
    sf::st_make_valid()

  # Extract habitat data for pixels
  cells_stars <- stars::st_as_stars(habitat_cropped) %>%
    sf::st_transform(., terra::crs(add_var_raster))
  cells <- sf::st_as_sf(cells_stars, as_points = TRUE)

  hab_pts <- pixel_points %>%
    mutate(
      id = hexid,
      area = h3::hex_area(res = hex_resolution, unit = unit)
    ) %>% # is this km2 ok?? #Anna - not sure this is actually working
    sf::st_join(., cells, join = sf::st_nearest_feature) %>%
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

  return(hab_pts)
}
