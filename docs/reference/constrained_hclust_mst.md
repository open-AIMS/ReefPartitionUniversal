# Default habitat clustering function using adespatial::constr.hclust.

Take a dataframe of pixels containing `additional_variable_cols` values,
create a minimum spanning tree using `prepare_mst_edges()` and then
cluster pixels using pixel data and edges with
[`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md).
Any additional arguments for `prepare_mst_edges()` or
[`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
(excluding `distance_alpha`) can be included.

## Usage

``` r
constrained_hclust_mst(pixels, distance_alpha = 0.5, ...)
```

## Arguments

- pixels:

  data.frame. Contains values for X and Y coordinates, as well as
  `additional_variable_cols`.

- distance_alpha:

  float numeric. Weighting applied to the additional variable distance
  values when creating the distance matrix for clustering. This argument
  is not included in `...` for discoverability.

- ...:

  additional arguments. Additional arguments can be used here and will
  be passed onto `prepare_mst_edges()` and
  [`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
  functions. These arguments must be named. `distance_alpha` argument is
  not included in these additional arguments. For information on
  arguments available in these functions and default values when
  arguments are not used, see `prepare_mst_edges()` and
  [`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md).

## Value

data.frame of pixels with allocated site_ids based on cluster outputs
from
[`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
using `prepare_mst_edges` to create a minimum spanning tree for input.
`site_id` values are a combination of the `id_col` value, `habitat_col`
value and the cluster allocation.
