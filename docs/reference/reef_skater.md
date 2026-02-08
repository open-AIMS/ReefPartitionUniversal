# Cluster pixels together using the spdep::skater algorithm.

Take a dataframe of pixels containing geometries of pixels and
`additional_variable_cols` values and cluster using the skater
algorithm. This clustering is performed top-down the costs of pruning
each minimum spanning tree edge. Only pixels connected by `edges` are
able to cluster together. For additional information see **insert
citation/link**.

## Usage

``` r
reef_skater(
  pixels,
  n_clust = round(min(10000, nrow(pixels))/200),
  site_size = 250 * 250,
  x_col = "X_standard",
  y_col = "Y_standard",
  habitat_col = "habitat",
  id_col = "UNIQUE_ID",
  additional_variable_cols = c("depth_standard"),
  parallelisation = "Windows",
  hex_resolution = 12
)
```

## Arguments

- pixels:

  data.frame. Contains values for X and Y coordinates, as well as
  `additional_variable_cols`.

- n_clust:

  integer numeric. Number of clusters in result output. (Point to cut
  hierarchical clustering tree). Default = (round(nrow(pixels) / 200))
  (dividing habitat into clusters containing an average of 200 pixels).

- site_size:

  numeric. Desired site size (area in m^2). Default = 625,000 (250m x
  250m).

- x_col:

  character. Name of the column holding X coordinates. Default =
  "X_standard".

- y_col:

  character. Name of the column holding Y coordinates. Default =
  "Y_standard".

- habitat_col:

  character. Column holding unique habitat values (attached to `id_col`
  value and site_id values on output). Default = "habitat".

- id_col:

  character. Column holding ID value for the target reef (attached to
  the site_id values on output). Default = "UNIQUE_ID".

- additional_variable_cols:

  character vector. Names of additional columns to contribute to the
  distance matrix. Default = c("depth_standard").

- parallelisation:

  character. Current option is only "Windows", using this option sets up
  a parallel::Cluster using detectCores() - 2 cores. This parallelises
  prunecost calculations within spdep::skater(). If `parallelisation` is
  not set to "Windows", no parallelisation will occur. Default =
  "Windows".

- hex_resolution:

  integer numeric. H3 hexagon resolution used in pixel creation.

## Value

data.frame of pixels with allocated site_ids based on cluster outputs.
`site_id` values are a combination of the `id_col` value, `habitat_col`
value and the cluster allocation.
