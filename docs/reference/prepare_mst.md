# Create a minimum spanning tree from geographic coordinates of pixels and extracted data.

Take a dataframe of pixels `pixels` and create a minimum spanning tree
between pixel coordinates, using edge costs that are a combination of
geographic distances and distances between pixels by
`additional_variable_cols` values. For additional details on minimum
spanning tree creation see igraph::mst().

## Usage

``` r
prepare_mst(
  pixels,
  additional_variable_cols = c("depth_standard"),
  mst_alpha = 0.5,
  hex_resolution = 12
)
```

## Arguments

- pixels:

  sf data.frame. Holds values for pixel geometries and
  `additional_variable_cols`

- additional_variable_cols:

  character vector. Names of the columns to extract additional
  (non-geometric) data from for cost weighting.

- mst_alpha:

  float numeric. Weighting applied to `additional_variable_cols`
  distance in edge cost weighting when combining with geographic
  distances. (1 - alpha) weight is applied to the geographic distance.
  Default = 0.5 (same weight for `additional_variable_cols` and
  geographic distances).

- hex_resolution:

  integer. H3 hex resolution of pixels. Default = 12.

## Value

igraph::mst Minimum spanning tree object.
