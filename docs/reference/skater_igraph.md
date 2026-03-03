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
