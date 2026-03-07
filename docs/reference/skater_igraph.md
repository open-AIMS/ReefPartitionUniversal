# An approximate Skater algorithm implemented using igraph methods.

Leverages C implementations in igraph package. The approach collects all
edges from all clusters, sorts by global cost, and iterates until a
valid split is found.

## Usage

``` r
skater_igraph(
  edges,
  data,
  ncuts,
  crit,
  vec.crit,
  method = "euclidean",
  p = 2,
  cov = NULL,
  inverted = FALSE
)
```

## Arguments

- edges:

  igraph edges. Graph edges of the minimum spanning tree object as
  returned by igraph::as_edgelist().

- data:

  dataframe (columns of numeric values). Dataframe columns containing
  values for each graph node of the desired variables (referred to as
  additional_variable_cols elsewhere).

- ncuts:

  integer. The number of cuts to make in the minimum spanning tree to
  generate the desired number of clusters.

- crit:

  integer. The desired number of nodes required to form a cluster.
  Should align with ncuts to approximate the total number of data points
  in the input data.

- vec.crit:

  numeric vector. The minimum and maximum crit values. If this argument
  is missing it defaults to c(crit, Inf), meaning no maximum cluster
  size.

- method:

  character. Distance calculation method passed onto
  [`ssw()`](https://open-aims.github.io/ReefPartitionUniversal/reference/ssw.md)
  for computing edge costs. Euclidean, manhattan and mahalanobis are
  internally defined options, other options are passed onto
  [`dist()`](https://rdrr.io/r/stats/dist.html).

- p:

  numeric. Power of the Minkowski distance, as in
  [`spdep::ssw()`](https://r-spatial.github.io/spdep/reference/ssw.html).
  Default = 2.

- cov:

  numeric matrix. Covariance matrix used to compute mahalanobis distance
  in ssw. Only required if method="mahalanobis". Default = NULL.

- inverted:

  Bool. If 'TRUE' `cov` is supposed to contain the inverse of the
  covariance matrix.
