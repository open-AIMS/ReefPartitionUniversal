# Cluster points using standard hierarchical clustering on geographic distance only (no depth, no spatial contiguity constraint).

Take a dataframe of points and cluster them using
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) on a geographic
(X/Y) distance matrix only - no depth or other additional variable
contributes to the distance matrix, and no minimum spanning tree is
built. Unlike
[`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
/
[`constrained_hclust_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust_mst.md),
no `links`/contiguity graph is used either: this is plain, unconstrained
agglomerative clustering, so at each merge step any two clusters can be
joined purely because they are close in X/Y distance, regardless of what
(if anything) lies between them. Resulting sites are therefore not
guaranteed to be spatially contiguous, unlike every other clustering
function in this package.

## Usage

``` r
hclust_geo(
  points,
  x_col = "X_standard",
  y_col = "Y_standard",
  id_col = "UNIQUE_ID",
  habitat_col = "habitat",
  distance_method = "euclidean",
  n_points = 204,
  n_clust = round(nrow(points)/n_points),
  method = "ward.D2",
  interpolation_threshold = 30000
)
```

## Arguments

- points:

  sf data.frame. Contains geometries and `x_col`/`y_col` coordinate
  columns.

- x_col:

  character. Name of the column holding X coordinates. Default =
  "X_standard".

- y_col:

  character. Name of the column holding Y coordinates. Default =
  "Y_standard".

- id_col:

  character. Column holding ID value for the target reef (attached to
  the site_id values on output). Default = "UNIQUE_ID".

- habitat_col:

  character. Column holding unique habitat values (attached to `id_col`
  value and site_id values on output). Default = "habitat".

- distance_method:

  character. Distance matrix creation method for the geographic
  coordinates. Default = "euclidean" (see
  [`dist()`](https://rdrr.io/r/stats/dist.html)).

- n_points:

  integer numeric. Desired number of points per cluster. Used to
  calculate `n_clust` (number of output clusters). Default = 204.

- n_clust:

  integer numeric. Number of clusters in result output. (Point to cut
  hierarchical clustering tree). Default =
  `round(nrow(points) / n_points)`.

- method:

  character. Agglomeration method passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html). Default =
  "ward.D2".

- interpolation_threshold:

  numeric. Habitats with more points than this are subsampled to
  `interpolation_threshold` points for clustering, then remaining points
  are assigned via nearest-neighbour interpolation - same handling
  [`constrained_hclust_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust_mst.md)
  uses for large habitats. Default = 30000.

## Value

data.frame of points with allocated site_ids based on cluster outputs.
`site_id` values are a combination of the `id_col` value, `habitat_col`
value and the cluster allocation.
