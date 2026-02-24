#' Cluster points into sites based on geographical attributes and additional extracted point values.
#'
#' @description From a dataframe of points and point data, this function clusters points into
#'   sites (within each habitat type), using a user defined clustering method that considers
#'   both geographical data and additional extracted data (such as depth).
#'
#' @param points data.frame. Contains a row for each point and values for each variable to
#'   be input into `habitat_clustering_function`.
#' @param habitat_clustering_function function. A function that takes a dataframe of points
#'   for a single habitat type as the first argument and additional arguments, and returns
#'   a dataframe of input points with an additional column containing assigned `site_id` values.
#'   Additional arguments must include `px_per_cluster`, `habitat_col`, `x_col`, `y_col`
#'   and `additional_variable_cols`.
#'   Default options available include `reef_skater` and `constrained_hclust`.
#' @param habitat_col character or integer. Column containing the categorical habitat allocations
#'   of points. Default = "habitat".
#' @param x_col character or integer. Column containing the continuous geographical X values
#'   for points. Default = "X".
#' @param y_col character or integer. Column containing the continuous geographical Y values
#'   for points. Default = "Y".
#' @param additional_variable_cols character vector. Vector containing column names for columns
#'   with additional clustering variable values. This vector can have length > 1. Values will
#'   be standardised and output as `*variable*_standard`. Default = c("Depth").
#' @param reef_id_col character or integer. Column containing the single unique reef ID to
#'   be assigned as an identifier before outputting cluster allocations. Default = "UNIQUE_ID".
#'
#' @return data.frame containing points that have a site ID allocated in `site_id` column.
#'   Output data includes clustering execution time.
#'
#' @export
#'
cluster_reef_points <- function(
  points,
  habitat_col = "habitat",
  x_col = "X",
  y_col = "Y",
  additional_variable_cols = c("depth"),
  reef_id_col = "UNIQUE_ID",
  habitat_clustering_function = reef_skater_fast,
  clustering_function_args = list()
) {
  if (nrow(points) < 1) {
    warning("Input dataframe contains no rows, returning input dataframe.")
    return(points)
  }

  # Formatting input data
  points <- dplyr::distinct(points) # Remove duplicate point rows
  reef_id <- unique(points[, reef_id_col, drop = TRUE])

  # Scale clustering variables including any additional variables added to points
  points$X_standard <- scale(points[, x_col, drop = TRUE])
  points$Y_standard <- scale(points[, y_col, drop = TRUE])

  for (variable in additional_variable_cols) {
    points[, paste0(variable, "_standard")] <- scale(points[,
      variable,
      drop = TRUE
    ])
  }

  habitat_list <- split(points, points[, habitat_col, drop = TRUE])

  # Make Clusters
  reef_start_time <- Sys.time()
  points_clustered <- lapply(
    habitat_list,
    function(habitat_points) {
      do.call(
        habitat_clustering_function,
        append(list(points = habitat_points), clustering_function_args)
      )
    }
  )
  reef_end_time <- Sys.time()
  points_clustered <- do.call(rbind, points_clustered)

  # Assign ID and execution time
  points_clustered[, reef_id_col] <- reef_id
  points_clustered[, "clustering_time"] <- reef_end_time - reef_start_time

  points_clustered$depth_extract <- points$depth_extract

  points_clustered
}
