#' Convert points for an entire reef that have been allocated site IDs into site polygons.
#'
#' @description From a dataframe of points and point data, this function clusters points into
#'   sites (within each habitat type), using a user defined clustering method that considers
#'   both geographical data and additional extracted data (such as depth). **cells**_to_polygons
#'   refers to points that have been extracted using H3 methods. This method of polygon
#'   creation uses H3 methods to polygonise covered areas.
#'
#' @param clustered_points data.frame. Contains a row for each point and cluster allocations.
#' @param site_id_col character or integer. Column containing site allocations for points.
#'   Default = "site_id".
#' @param reef_cols_to_keep character vector. Columns containing unique values per reef that
#'   can be allocated to the collated site polygons to conserve IDs and other reef level
#'   information. Examples include reef level mean depth, or reef distance to coastline.
#'   Default = c("clustering_time", "UNIQUE_ID").
#'
#' @return sf data.frame containing site polygons created from points using allocated
#'   `site_id_col` values.
#'
#' @export
#'
cells_to_polygons <- function(
  clustered_points,
  site_id_col = "site_id",
  reef_cols_to_keep = c("clustering_time", "UNIQUE_ID")
) {
  if (nrow(clustered_points) < 1) {
    warning("Input dataframe contains no rows, returning input dataframe.")
    return(clustered_points)
  }

  point_cluster_list <- split(
    clustered_points,
    clustered_points[, site_id_col, drop = TRUE]
  )

  # use function 'group_hex' to group hexagons into polygon or multipolygon
  #sites <- lapply(point_cluster_list, hex_to_polygons)
  sites <- lapply(point_cluster_list, point_to_polygons)

  sites <- do.call(rbind, sites)

  for (col in reef_cols_to_keep) {
    sites[, col] <- unique(clustered_points[, col, drop = TRUE])
  }

  sites
}

#' Helper function to convert points in a single site into an sf poylgon.
#'
#' @param x data.frame. Contains point values for the target site.
#' @param h3_id_col character or integer. Column containing the H3 ID values for each point
#'   to be collated into the polygon. Default = "id".
#' @param site_id_col charcater vector. Column containing the unique site ID for the target
#'   site contained in `x`. Default = "site_id".
#'
#' @return sf data.frame containing site polygons created from points using allocated
#'   `site_id_col` values.
#'
hex_to_polygons <- function(x, h3_id_col = "id", site_id_col = "site_id") {
  site_polygon <- h3::h3_set_to_multi_polygon(x[, h3_id_col, drop = TRUE]) %>%
    sf::st_buffer(dist = 0) %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry = x)

  site_polygon[, site_id_col] <- unique(x[, site_id_col, drop = TRUE])

  site_polygon
}

#' Convert clustered points into collated site polygons
#'
#' @description From a dataframe of points, this function collates points into
#'   polygons depending on their `site_id_col` assignment. This method uses the
#'   initial resolution of points (i.e. resolution of the habitat raster used for
#'   extraction) to give points 2D space before being converted into polygons.
#'   **pixels**_to_polygons refers to points that have been extracted using raster
#'   pixel resolution, rather than H3 cells. This method uses raster cells to polygonise
#'   the covered areas, rather than H3 methods.
#'
#'
#' @param points data.frame. Contains a row for each point and cluster allocations.
#' @param site_id_col character or integer. Column containing site allocations for points.
#'   Default = "site_id".
#' @param pixel_size numeric. Resolution of original habitat raster cells used to
#'   extract point data. Must be in the same units as `x_col`/`y_col`.
#' @param x_col character or integer. Column containing x coordinates for points.
#' @param y_col character or integer. Column containing y coordinates for points.
#' @param id_col tidyselect. Column containing unique ID for the reef to be attached
#'   to outputs.
#' @param habitat_col tidyselect. Column containing habitat categories.
#' @param additional_variable_cols tidyselect. Column(s) containing additional
#'   continuous variables to summarise for points within sites. Output will include
#'   site polygon median and standard deviation values for all columns selected.
#'
#' @return sf data.frame containing site polygons created from points using allocated
#'   `site_id_col` values.
#'
#' @export
#'
pixels_to_polygons <- function(
  points,
  id_col,
  habitat_col,
  additional_variable_cols,
  site_id_col = "site_id",
  pixel_size = 10,
  x_col = "X",
  y_col = "Y"
) {
  cols <- tidyselect::eval_select(
    rlang::enquo(additional_variable_cols),
    points
  )

  # Get the grid extent
  x_coords <- sort(unique(points$X))
  y_coords <- sort(unique(points$Y))

  # Create raster template
  r <- terra::rast(
    extent = terra::ext(
      min(x_coords) - pixel_size / 2,
      max(x_coords) + pixel_size / 2,
      min(y_coords) - pixel_size / 2,
      max(y_coords) + pixel_size / 2
    ),
    resolution = pixel_size,
    crs = terra::crs(points)
  )

  # Create site ID raster
  point_site_ids <- factor(points$site_id)
  site_id_levels <- levels(point_site_ids)

  points$site_id_num <- as.numeric(point_site_ids)
  site_raster <- terra::rasterize(terra::vect(points), r, field = "site_id_num")

  # Convert to polygons - THIS AUTOMATICALLY DISSOLVES
  site_polys <- terra::as.polygons(site_raster) %>%
    sf::st_as_sf()

  # Find the column name that contains the raster values
  raster_col_name <- names(site_polys)[1] # First column is usually the raster value

  # Rename it to site_id_num
  site_polys <- site_polys %>%
    dplyr::rename(site_id_num = !!raster_col_name)
  site_polys$site_id <- site_id_levels[site_polys$site_id_num]

  # Join back attributes
  site_attrs <- points %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(site_id) %>%
    dplyr::summarise(
      habitat = first({{ habitat_col }}),
      UNIQUE_ID = first({{ id_col }}),
      n_cells = nrow(points),
      dplyr::across(
        {{ cols }},
        list(median = median, sd = sd),
        na.rm = TRUE
      )
    )

  site_polygon <- site_polys %>%
    dplyr::left_join(site_attrs, by = "site_id") %>%
    sf::st_make_valid()

  site_polygon
}
