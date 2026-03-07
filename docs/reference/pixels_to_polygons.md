# Convert clustered points into collated site polygons

From a dataframe of points, this function collates points into polygons
depending on their `site_id_col` assignment. This method uses the
initial resolution of points (i.e. resolution of the habitat raster used
for extraction) to give points 2D space before being converted into
polygons. **pixels**\_to_polygons refers to points that have been
extracted using raster pixel resolution, rather than H3 cells. This
method uses raster cells to polygonise the covered areas, rather than H3
methods.

## Usage

``` r
pixels_to_polygons(
  points,
  id_col,
  habitat_col,
  additional_variable_cols,
  site_id_col = "site_id",
  pixel_size = 10,
  x_col = "X",
  y_col = "Y"
)
```

## Arguments

- points:

  data.frame. Contains a row for each point and cluster allocations.

- id_col:

  tidyselect. Column containing unique ID for the reef to be attached to
  outputs.

- habitat_col:

  tidyselect. Column containing habitat categories.

- additional_variable_cols:

  tidyselect. Column(s) containing additional continuous variables to
  summarise for points within sites. Output will include site polygon
  median and standard deviation values for all columns selected.

- site_id_col:

  character or integer. Column containing site allocations for points.
  Default = "site_id".

- pixel_size:

  numeric. Resolution of original habitat raster cells used to extract
  point data. Must be in the same units as values in `x_col`/`y_col`.

- x_col:

  character or integer. Column containing x coordinates for points.

- y_col:

  character or integer. Column containing y coordinates for points.

## Value

sf data.frame containing site polygons created from points using
allocated `site_id_col` values.
