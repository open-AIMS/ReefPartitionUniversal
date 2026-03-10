# Convert vector data for a reef(s) to raster data for pixel extraction.

Using terra this function converts vector data to raster data for use in
pixel extraction steps. This function is intended for use with vector
data obtained from the Allen Coral Atlas. This conversion requires a
template raster object to fill with values. If raster data is already
available, for example a continuous raster of depth values for
extraction, then it is recommended to use this raster as a template for
rasterization rather than the default generated template. Rasterization
can take a long time and a large amount of RAM if the `raster_template`
is fine resolution.

## Usage

``` r
vector_to_raster(
  vector_data,
  data_column,
  output_file = NA,
  raster_template = generate_raster_template(vector_data)
)
```

## Arguments

- vector_data:

  sf data.frame. Contains geometries and the values to convert to raster
  format

- data_column:

  character. Column name of data to be filled into the output raster. If
  the values in the column are character class, then the output raster
  will contain numeric factor levels. The corresponding levels will be
  output in a csv file by the name `*output_file*_levels.csv`.

- output_file:

  character. `*.tif` output file path for saving of raster data.

- raster_template:

  SpatRaster. Raster object to use in terra::rasterize when converting
  data. By default this raster template is generated using
  `generate_raster_template(vector_data)`, however it is recommended to
  use existing raster data as a template if available.

## Value

SpatRaster conversion of `vector_data` using a `raster_template`. Output
raster is also saved to `output_file`, and a csv containing numerical
allocations is saved if the values in `data_column` are characters. If
`data_column` holds character data, then the levels assigned will also
be returned (using a named list to hold both objects).
