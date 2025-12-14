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
){
    if (nrow(pixels) < 1) {
        warning("Input dataframe contains no rows, returning input dataframe.")
        return(pixels)
    }

    pixel_cluster_list <- split(clustered_pixels, clustered_pixels[, site_id_col])
    
    #use function 'group_hex' to group hexagons into polygon or multipolygon
    sites <- lapply(pixel_cluster_list, hex_to_polygons)
    
    sites = do.call(rbind, sites)

    for (col in reef_cols_to_keep) {
        sites[, col] <- unique(clustered_pixels[, col])
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
hex_to_polygons <- function(x, h3_id_col = "id", site_id_col = "site_id"){
  site_polygon <- h3_set_to_multi_polygon(x[, h3_id_col]) %>%
    st_buffer(dist = 0) %>%
    st_as_sf() %>%
    rename(geometry = x)

    site_polygon[, site_id_col] <- unique(x[, site_id_col])

  site_polygon
}