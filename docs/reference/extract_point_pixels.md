# Extract pixel data from selected habitats over a target reef.

Extract data from the centroids of pixels that overlap the target
`reef_polyon`. Extraction is performed for two raster layers, one
`habitat_raster` must contain habitat categories that form the basis of
the pixel centroids, the other `add_var_raster` is additional desired
data for clustering at later points in the workflow. Extrction method
uses habitat_raster pixels, meaning points have a minimum distance apart
of res(habitat_raster) / 2.

## Usage

``` r
extract_point_pixels(
  reef_polygon,
  habitat_raster,
  add_var_raster,
  habitat_categories,
  additional_variable_name = "depth",
  output_epsg = 3112,
  interpolation = TRUE
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

- additional_variable_name:

  character. Name to assign the extracted data from `add_var_raster`.
  Default = "depth".

- output_epsg:

  integer. EPSG code used for outputting pixels and extracted data.
  Default = 3112.

- interpolation:

  bool. Option to interpolate missing values after extracting data from
  `add_var_raster` using nearest neighbour interpolation.

## Value

data.frame containing points for pixels from `habitat_raster` covering
`reef_polygon` for selected habitats in `habitat_categories`, alongside
extracted data from `add_var_raster`.
