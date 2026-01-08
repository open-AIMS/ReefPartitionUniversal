#' Cluster pixels into sites based on geographical attributes and additional extracted pixel values.
#'
#' @description From a dataframe of pixels and pixel data, this function clusters pixels into
#'   sites (within each habitat type), using a user defined clustering method that considers
#'   both geographical data and additional extracted data (such as depth).
#'
#' @param pixels data.frame. Contains a row for each pixel and values for each variable to
#'   be input into `habitat_clustering_function`.
#' @param habitat_clustering_function function. A function that takes a dataframe of pixels
#'   for a single habitat type as the first argument and additional arguments, and returns
#'   a dataframe of input pixels with an additional column containing assigned `site_id` values.
#'   Additional arguments must include `px_per_cluster`, `habitat_col`, `x_col`, `y_col`
#'   and `additional_variable_cols`.
#'   Default options available include `reef_skater` and `constrained_hclust`.
#' @param px_per_cluster integer. The number of pixels to be allocated into each cluster using
#'   `habitat_clustering_function`. If `habitat_clustering_function` defines k-clusters, this
#'   must be `round(nrow(x) / px_per_cluster)`. Default = 200 pixels.
#' @param habitat_col character or integer. Column containing the categorical habitat allocations
#'   of pixels. Default = "habitat".
#' @param x_col character or integer. Column containing the continuous geographical X values
#'   for pixels. Default = "X".
#' @param y_col character or integer. Column containing the continuous geographical Y values
#'   for pixels. Default = "Y".
#' @param additional_variable_cols character vector. Vector containing column names for columns
#'   with additional clustering variable values. This vector can have length > 1. Values will
#'   be standardised and output as `*variable*_standard`. Default = c("Depth").
#' @param reef_id_col character or integer. Column containing the single unique reef ID to
#'   be assigned as an identifier before outputting cluster allocations. Default = "UNIQUE_ID".
#'
#' @return data.frame containing pixels that have a site ID allocated in `site_id` column.
#'   Output data includes clustering execution time.
#'
#' @export
#'
cluster_reef_pixels <- function(
    pixels,
    habitat_clustering_function,
    habitat_col = "habitat",
    x_col = "X",
    y_col = "Y",
    additional_variable_cols = c("depth"),
    reef_id_col = "UNIQUE_ID") {
  if (nrow(pixels) < 1) {
    warning("Input dataframe contains no rows, returning input dataframe.")
    return(pixels)
  }

  # Formatting input data
  pixels <- dplyr::distinct(pixels) # Remove duplicate pixel rows
  reef_id <- unique(pixels[, reef_id_col, drop = TRUE])

  # Scale clustering variables including any additional variables added to pixels
  pixels$X_standard <- scale(pixels[, x_col, drop = TRUE])
  pixels$Y_standard <- scale(pixels[, y_col, drop = TRUE])

  for (variable in additional_variable_cols) {
    pixels[, paste0(variable, "_standard")] <- scale(pixels[, variable, drop = TRUE])
  }

  habitat_list <- split(pixels, pixels[, habitat_col, drop = TRUE])

  # Make Clusters
  reef_start_time <- Sys.time()
  pixels_clustered <- lapply(habitat_list, function(x) habitat_clustering_function(x))
  reef_end_time <- Sys.time()
  pixels_clustered <- do.call(rbind, pixels_clustered)

  # Assign ID and execution time
  pixels_clustered[, reef_id_col] <- reef_id
  pixels_clustered[, "clustering_time"] <- reef_end_time - reef_start_time
  
   pixels_clustered$depth_extract <- pixels$depth_extract

  pixels_clustered
}
