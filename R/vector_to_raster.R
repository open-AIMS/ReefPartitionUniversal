#' Generate a raster template based on sf `vector_data` at a specific `pixel_resolution`.
#'
#' @param vector_data sf data.frame. Vector data to generate a raster template object.
#' @param pixel_resolution numeric. Resolution of raster cells to use in template
#'   creation. Default is NA. If pixel_resolution is NA then a default resolution
#'   of 0.001 degrees or 100m will be used depending on the unit of `vector_data`
#'   coordinate reference system. This default resolution is used to prevent
#'   long execution times and over-use of RAM.
#'
#' @return template raster object based on vector_data extent.
#'
#' @export
#'
generate_raster_template <- function(vector_data, pixel_resolution = NA) {
  if (is.na(pixel_resolution)) {
    if (sf::st_crs(vector_data)$units_gdal == "degree") {
      pixel_resolution <- 0.001 # If vector data CRS is in degrees default to 0.001 resolution (*roughly* 100m)
    } else {
      pixel_resolution <- 100 # If vector data CRS is not in degrees default to 100 (metres assumed)
    }
  }

  bbox <- sf::st_bbox(vector_data)
  template <- terra::rast(
    extent = terra::ext(
      bbox["xmin"],
      bbox["xmax"],
      bbox["ymin"],
      bbox["ymax"]
    ),
    resolution = pixel_resolution,
    crs = terra::crs(vector_data)
  )

  return(template)
}

#' Convert vector data for a reef(s) to raster data for pixel extraction.
#'
#' @description Using terra this function converts vector data to raster data
#'   for use in pixel extraction steps. This function is intended for use with
#'   vector data obtained from the Allen Coral Atlas. This conversion requires
#'   a template raster object to fill with values. If raster data is already
#'   available, for example a continuous raster of depth values for extraction,
#'   then it is recommended to use this raster as a template for rasterization
#'   rather than the default generated template.
#'   Rasterization can take a long time and a large amount of RAM if the `raster_template`
#'   is fine resolution.
#'
#' @param vector_data sf data.frame. Contains geometries and the values to convert
#'   to raster format
#' @param data_column character. Column name of data to be filled into the output
#'   raster. If the values in the column are character class, then the output
#'   raster will contain numeric factor levels. The corresponding levels will be
#'   output in a csv file by the name `*output_file*_levels.csv`.
#' @param output_file character. `*.tif` output file path for saving of raster data.
#' @param raster_template SpatRaster. Raster object to use in terra::rasterize when
#'   converting data. By default this raster template is generated using
#'   `generate_raster_template(vector_data)`, however it is recommended to use
#'   existing raster data as a template if available.
#'
#' @return SpatRaster conversion of `vector_data` using a `raster_template`. Output
#'   raster is also saved to `output_file`, and a csv containing numerical allocations
#'   is saved if the values in `data_column` are characters.
#'
#' @export
#'
aca_vector_to_raster <- function(
  vector_data,
  data_column,
  output_file,
  raster_template = generate_raster_template(vector_data)
) {
  if (file.exists(output_file)) {
    rlang::abort("Output raster already exists.", class = "file_exists")
  }

  column_name <- data_column
  if (class(vector_data[, data_column, drop = TRUE]) == "character") {
    column_name <- paste0(data_column, "_num")

    value_levels <- levels(factor(vector_data[, data_column, drop = TRUE]))

    value_levels <- data.frame(
      character = value_levels,
      numeric_levels = seq_along(value_levels)
    )
    vector_data[, column_name] <- as.numeric(factor(vector_data[,
      data_column,
      drop = TRUE
    ]))

    write.csv(
      value_levels,
      gsub(x = output_file, pattern = ".tif", replacement = "_levels.csv"),
      row.names = FALSE
    )
  }

  raster_output <- terra::rasterize(
    x = terra::vect(vector_data[, column_name]),
    y = raster_template,
    field = column_name,
    background = NA,
    touches = TRUE,
    filename = output_file
  )

  return(raster_output)
}
