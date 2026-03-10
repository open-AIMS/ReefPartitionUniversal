# target reef.

Checking input data formats and values for the target reef area are
valid. This includes checking that all input data share the same CRS,
checking that all input raster extents intersect with the `reef_polygon`
area, and checking that all input raster data have some valid values
intersecting the reef polygon. This function is automatically called in
`extract_pixel_points`, but can also be run before extraction to ensure
inputs will be compatible ( e.g. as a filter for invalid reefs).

## Usage

``` r
input_check(reef_polygon, habitat_raster, add_var_raster, habitat_categories)
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

  vector. Vector containing target habitat categories. This is used to
  check that `habitat_raster` contains pixels within `reef_polygon` that
  have valid target category values.

## Value

Function errors and provides message if checks are not met. If all
checks are met, the function will not error, but return NULL.
