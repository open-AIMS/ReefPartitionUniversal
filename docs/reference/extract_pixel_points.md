# Extract pixel data from selected habitats over a target reef.

Extract data from the centroids of pixels that overlap the target
`reef_polyon`. Extraction is performed for two raster layers, one
`habitat_raster` must contain habitat categories that form the basis of
the pixel centroids, the other `add_var_raster` is additional desired
data for clustering at later points in the workflow.

## Usage

``` r
extract_pixel_points(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  hex_resolution = 12,
  unit = "km2",
  additional_variable_name = "depth",
  output_epsg = 3112,
  resample_method = "bilinear"
)
```

## Arguments

- reef_polygon:

  sf_object. sf object containing the target reef polygons for data
  coverage.

- habitat_raster:

  SpatRaster. Terra raster object containing categorical values for
  habitats covering the target reef area.

- add_var_raster:

  SpatRaster. Terra raster object containing an additional variable to
  extract for all selected habitat pixels covering the target reef area.
  Can be continuous or categorical, however the raster will be resampled
  and the method should be changed for categorical data.

- habitat_categories:

  character. Vector containing the habitat categories to select from
  `habitat_raster`.

- hex_resolution:

  integer. Selected H3 resolution of hexagons for pixel representation.
  Default = 12.

- unit:

  character. Unit used by H3 functions to calculate the area of
  hexagons. Default = "km2".

- additional_variable_name:

  character. Name to assign the extracted data from `add_var_raster`.
  Default = "depth".

- output_epsg:

  integer. EPSG code used for outputting pixels and extracted data.
  Default = 3112.

- resample_method:

  Method used to resample `add_var_raster` before extracting pixel data.
  Default = "bilinear", method should be changed for categorical data.
  See
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html)
  for more details.

## Value

data.frame containing `habitat_raster` pixels covering `reef_polygon`
for selected habitats in `habitat_categories`, alongside extracted data
from `add_var_raster`.
