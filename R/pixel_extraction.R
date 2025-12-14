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
    resample_method = "bilinear") {
  # Perform input data checks before proceeding with computations

  # Ensure that the reef_polygon and categorical habitat raster have the same CRS
  reef_crs <- st_crs(reef_polygon)
  if (st_crs(habitat_raster) != reef_crs) {
    stop("Ensure st_crs(habitat_raster) == st_crs(reef_polygon) before proceeding.")
  }

  # Ensure that the habitat raster and additional variable raster have the same CRS
  if (st_crs(habitat_raster) != st_crs(add_var_raster)) {
    stop("Ensure st_crs(habitat_raster) == st_crs(add_var_raster) before proceeding.")
  }

  # Check if reef_polygon intersects with the habitat_raster
  hab_raster_bbox <- st_bbox(habitat_raster)
  habitat_intersects <- st_intersects(st_as_sfc(hab_raster_bbox), reef_polygon, sparse = FALSE)

  if (!any(habitat_intersects)) {
    stop("
            Ensure that reef_polygon intersects with habitat_raster.
            `st_intersects(st_as_sfc(st_bbox(habitat_raster))), reef_polygon, sparse = FALSE)`
        ")
  }

  # Check if reef_polygon intersects with the habitat_raster
  add_var_raster_bbox <- st_bbox(add_var_raster)
  add_var_intersects <- st_intersects(st_as_sfc(add_var_raster_bbox), reef_polygon, sparse = FALSE)

  if (!any(add_var_intersects)) {
    stop("
            Ensure that reef_polygon intersects with add_var_raster.
            `st_intersects(st_as_sfc(st_bbox(add_var_raster))), reef_polygon, sparse = FALSE)`
        ")
  }

  # Crop the input raster layers for faster extraction
  reef_polygon_terra <- vect(reef_polygon)
  habitat_cropped <- crop(habitat_raster, reef_polygon_terra)
  add_var_cropped <- crop(add_var_raster, reef_polygon_terra)

  if (ncell(habitat_cropped) == 0) {
    stop("Cropping resulted in empty raster - check if reef_polygon overlaps with valid raster data.")
  }

  names(habitat_cropped)[1] <- "categorical_habitat"

  # Filter just the desired habitat type pixels
  habitat_cropped[!(habitat_cropped %in% habitat_categories)] <- NA
  # Filter just pixels that overlap the target reef.
  # Crop uses a bounding box, so must be followed with mask.
  habitat_cropped <- mask(habitat_cropped, reef_polygon_terra)

  # Extract habitat pixels
  hexid <- terra::as.data.frame( # Extracts pixel centroid points from the raster data
    habitat_cropped,
    xy = TRUE,
    na.rm = TRUE
  ) %>%
    st_as_sf(., coords = c("x", "y"), crs = reef_crs) %>%
    rename(class = "categorical_habitat") %>%
    st_cast("POINT") %>%
    geo_to_h3(., res = hex_resolution) # Convert pixel centroid points to hexagons ID format.

  hexid <- unique(hexid) # Remove pixels with the same coordinates
  pixel_points <- h3_to_geo_sf(hexid) # Get the centers of the given H3 indexes as sf object.

  if (length(hexid) < 2) {
    stop("Less than 2 pixels identified from inputs.")
  }

  # Extract values from the additional variable raster layer and attach them to points
  add_var_resampled <- disagg(add_var_cropped, fact = 5, method = resample_method)

  additional_var_values <- terra::extract(add_var_resampled, pixel_points, df = TRUE)
  colnames(additional_var_values)[2] <- additional_variable_name
  pixel_points[, additional_variable_name] <- additional_var_values[, additional_variable_name]

  # Remove points that have an invalid value for the additional variable
  pixel_points <- pixel_points[!is.na(pixel_points$Depth), ]
  hexid <- hexid[hexid %in% pixel_points$h3_index]

  # Clean up pixels and extracted data
  # Transform Pixels to match the datas' CRS (by default h3 points do not have a CRS)
  pixel_points <- st_transform(pixel_points, reef_crs)

  pixel_points <- pixel_points %>%
    filter(!is.na(st_dimension(.))) %>% # Remove NA dimensions
    st_make_valid()
 
  # Extract habitat data for pixels
  cells <- st_as_sf(as.polygons(habitat_cropped), as_points = TRUE)

  hab_pts <- pixel_points %>%
    mutate(
      id = hexid,
      area = hex_area(res = hex_resolution, unit = unit)
    ) %>% # is this km2 ok?? #Anna - not sure this is actually working
    st_join(., cells, join = st_nearest_feature) %>%
    rename(geomorph = "categorical_habitat") %>%
    st_transform(output_epsg) %>% # project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
    bind_cols(., base::as.data.frame(st_coordinates(.))) %>%
    filter(!is.na(geomorph), if (!is.null(geozone_list)) geomorph %in% geozone_list else TRUE) %>% # Handle NULL geozone_list
    rename(habitat = geomorph)
}
