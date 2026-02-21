#" Check that input data are formatted correctly and contain valid values for the
#' target reef.
#'
#' @export
#'
input_check <- function(
  reef_polygon,
  habitat_raster,
  add_var_raster
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
      "Cropping resulted in empty habitat raster - check if reef_polygon overlaps with valid habitat raster data."
    )
  }

  if (all(is.na(values(add_var_cropped)))) {
    stop(
      "Cropping resulted in empty additional variable raster - check if reef_polygon overlaps with valid additional variable raster data."
    )
  }

  return(TRUE)
}
