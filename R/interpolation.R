# NA handler
fill_na_nearest <- function(pixel_data, columns) {
  for (col in columns) {
    if (any(is.na(pixel_data[[col]]))) {
      has_value <- which(!is.na(pixel_data[[col]]))
      has_na <- which(is.na(pixel_data[[col]]))
      
      if (length(has_value) == 0) next
      
      coords_with_value <- st_coordinates(st_centroid(pixel_data[has_value, ]))
      coords_with_na <- st_coordinates(st_centroid(pixel_data[has_na, ]))
      
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
