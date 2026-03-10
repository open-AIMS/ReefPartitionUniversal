# Cluster points together using an optimized SKATER algorithm.

Take a dataframe of points containing geometries of points and
`additional_variable_cols` values and cluster using the SKATER (Spatial
'K'luster Analysis by Tree Edge Removal) algorithm. This clustering is
performed by iteratively pruning edges from a minimum spanning tree
based on the costs of edge removal. Only points connected by edges in
the MST are able to cluster together. This implementation uses a hybrid
approach combining igraph graph operations with the original SKATER
algorithm logic for improved performance. For datasets exceeding 10,000
points, clustering is performed on a random sample with results
interpolated to remaining points via nearest neighbor assignment.

## Usage

``` r
reef_skater_fast(
  points,
  n_clust = NA,
  site_size = 250 * 250,
  x_col = "X_standard",
  y_col = "Y_standard",
  habitat_col = "habitat",
  id_col = "UNIQUE_ID",
  additional_variable_cols = c("depth_standard"),
  point_area = 100,
  interpolation_threshold = 30000,
  ...
)
```

## Arguments

- points:

  data.frame. Contains values for X and Y coordinates, as well as
  `additional_variable_cols`.

- n_clust:

  integer numeric. Number of clusters in result output. Default =
  (round(nrow(points) / 200)) (dividing habitat into clusters containing
  an average of 200 points).

- site_size:

  numeric. Desired site size (area in m^2). Used to calculate minimum
  cluster size constraint based on H3 hexagon resolution. Default = 250
  \* 250 (62,500 m^2).

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
  distance matrix for clustering. Default = c("depth_standard").

- point_area:

  integer numeric. Area implicitly occupied by each point. Should be in
  the same units as `site_size`. If using pixels extracted from raster
  data then this value is res(raster)

  - res(raster), if using H3 cells then this value is the H3 cell area
    determined by the hexagon resolution (e.g. resolution 12 = cell area
    307.2 m^2). Default point_area = 100 m^2.

- interpolation_threshold:

  numeric. Threshold from where to sample random points and interpolate
  clusters for remaining points. This value should be scaled with reef
  area for larger reefs. Default value is 30,000, setting a higher
  threshold may result in long computation times and high RAM usage.

- ...:

  additional arguments. Additional arguments can be used here and will
  be passed onto `prepare_mst_edges()` and
  [`skater_igraph()`](https://open-aims.github.io/ReefPartitionUniversal/reference/skater_igraph.md)
  functions. These arguments must be named. For information on arguments
  available in these functions and default values when arguments are not
  used, see `prepare_mst_edges()` and
  [`skater_igraph()`](https://open-aims.github.io/ReefPartitionUniversal/reference/skater_igraph.md).

## Value

data.frame of points with allocated site_ids based on cluster outputs.
`site_id` values are a combination of the `id_col` value, `habitat_col`
value and the cluster allocation.
