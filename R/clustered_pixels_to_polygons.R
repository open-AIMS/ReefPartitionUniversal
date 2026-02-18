#' Convert pixels for an entire reef that have been allocated site IDs into site polygons.
#'
#' @description From a dataframe of pixels and pixel data, this function clusters pixels into
#'   sites (within each habitat type), using a user defined clustering method that considers
#'   both geographical data and additional extracted data (such as depth).
#'
#' @param clustered_pixels data.frame. Contains a row for each pixel and cluster allocations.
#' @param site_id_col character or integer. Column containing site allocations for pixels.
#'   Default = "site_id".
#' @param reef_cols_to_keep character vector. Columns containing unique values per reef that
#'   can be allocated to the collated site polygons to conserve IDs and other reef level
#'   information. Examples include reef level mean depth, or reef distance to coastline.
#'   Default = c("clustering_time", "UNIQUE_ID").
#'
#' @return sf data.frame containing site polygons created from pixels using allocated
#'   `site_id_col` values.
#'
#' @export
#'
clustered_pixels_to_polygons <- function(
  clustered_pixels,
  site_id_col = "site_id",
  reef_cols_to_keep = c("clustering_time", "UNIQUE_ID")
) {
  if (nrow(clustered_pixels) < 1) {
    warning("Input dataframe contains no rows, returning input dataframe.")
    return(clustered_pixels)
  }

  pixel_cluster_list <- split(
    clustered_pixels,
    clustered_pixels[, site_id_col, drop = TRUE]
  )

  # use function 'group_hex' to group hexagons into polygon or multipolygon
  #sites <- lapply(pixel_cluster_list, hex_to_polygons)
  sites <- lapply(pixel_cluster_list, pixel_to_polygons)

  sites <- do.call(rbind, sites)

  for (col in reef_cols_to_keep) {
    sites[, col] <- unique(clustered_pixels[, col, drop = TRUE])
  }

  sites
}

#' Helper function to convert pixels in a single site into an sf poylgon.
#'
#' @param x data.frame. Contains pixel values for the target site.
#' @param h3_id_col character or integer. Column containing the H3 ID values for each pixel
#'   to be collated into the polygon. Default = "id".
#' @param site_id_col charcater vector. Column containing the unique site ID for the target
#'   site contained in `x`. Default = "site_id".
#'
#' @return sf data.frame containing site polygons created from pixels using allocated
#'   `site_id_col` values.
#'
hex_to_polygons <- function(x, h3_id_col = "id", site_id_col = "site_id") {
  site_polygon <- h3::h3_set_to_multi_polygon(x[, h3_id_col, drop = TRUE]) %>%
    sf::st_buffer(dist = 0) %>%
    sf::st_as_sf() %>%
    rename(geometry = x)

  site_polygon[, site_id_col] <- unique(x[, site_id_col, drop = TRUE])

  site_polygon
}

pixel_to_polygons <- function(x, site_id_col = "site_id") {
  library(terra)
  library(sf)
  library(dplyr)

  # Get the grid extent
  x_coords <- sort(unique(x$X))
  y_coords <- sort(unique(x$Y))
  pixel_size <- 10

  # Create raster template
  r <- rast(
    extent = ext(
      min(x_coords) - pixel_size / 2,
      max(x_coords) + pixel_size / 2,
      min(y_coords) - pixel_size / 2,
      max(y_coords) + pixel_size / 2
    ),
    resolution = pixel_size,
    crs = crs(x)
  )

  # Create site ID raster
  pixel_site_ids <- factor(x$site_id)
  site_id_levels <- levels(pixel_site_ids)

  x$site_id_num <- as.numeric(pixel_site_ids)
  site_raster <- rasterize(vect(x), r, field = "site_id_num")

  # Convert to polygons - THIS AUTOMATICALLY DISSOLVES
  site_polys <- as.polygons(site_raster) %>%
    st_as_sf()

  # Find the column name that contains the raster values
  raster_col_name <- names(site_polys)[1] # First column is usually the raster value

  # Rename it to site_id_num
  site_polys <- site_polys %>%
    rename(site_id_num = !!raster_col_name)
  site_polys$site_id <- site_id_levels[site_polys$site_id_num]

  # Join back attributes
  site_attrs <- x %>%
    st_drop_geometry() %>%
    group_by(site_id) %>%
    summarise(
      habitat = first(habitat),
      UNIQUE_ID = first(UNIQUE_ID),
      depth_med = median(depth, na.rm = TRUE),
      depth_sd = sd(depth, na.rm = TRUE),
      npixels = first(npixels),
      clustering_time = first(clustering_time),
      n_cells = nrow(x)
    )

  site_polygon <- site_polys %>%
    left_join(site_attrs, by = "site_id") %>%
    st_make_valid()

  # # Create pixel polygons with all attributes
  # site_polygon <- x %>%
  #   # First create individual pixel polygons
  #   mutate(
  #     pixel_id = row_number(),
  #     geometry_pixel = st_sfc(
  #       purrr::map2(X, Y, ~st_polygon(list(
  #         rbind(
  #           c(.x - 5, .y - 5),
  #           c(.x + 5, .y - 5),
  #           c(.x + 5, .y + 5),
  #           c(.x - 5, .y + 5),
  #           c(.x - 5, .y - 5)
  #         )
  #       ))),
  #       crs = st_crs(x)
  #     )
  #   ) %>%
  #   # Convert to sf object with pixel geometries
  #   st_as_sf(sf_column_name = "geometry_pixel") %>%
  #   # Group by site_id and union pixels
  #   group_by(site_id) %>%
  #   summarise(geometry = st_union(geometry_pixel)) %>%
  #   ungroup() %>%
  #   # Clean up geometries
  #   st_make_valid() %>%
  #   # Optionally cast to MULTIPOLYGON
  #   st_cast("MultiPOLYGON")

  #   site_polygon[, site_id_col] <- unique(x[, site_id_col, drop = TRUE])

  site_polygon
}
