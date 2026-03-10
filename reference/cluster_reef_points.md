# Cluster points into sites based on geographical attributes and additional extracted point values.

From a dataframe of points and point data, this function clusters points
into sites (within each habitat type), using a user defined clustering
method that considers both geographical data and additional extracted
data (such as depth).

## Usage

``` r
cluster_reef_points(
  points,
  habitat_col = "habitat",
  x_col = "X",
  y_col = "Y",
  additional_variable_cols = c("depth"),
  reef_id_col = "UNIQUE_ID",
  habitat_clustering_function = reef_skater_fast,
  clustering_function_args = list()
)
```

## Arguments

- points:

  data.frame. Contains a row for each point and values for each variable
  to be input into `habitat_clustering_function`.

- habitat_col:

  character or integer. Column containing the categorical habitat
  allocations of points. Default = "habitat".

- x_col:

  character or integer. Column containing the continuous geographical X
  values for points. Default = "X".

- y_col:

  character or integer. Column containing the continuous geographical Y
  values for points. Default = "Y".

- additional_variable_cols:

  character vector. Vector containing column names for columns with
  additional clustering variable values. This vector can have length
  \> 1. Values will be standardised and output as `*variable*_standard`.
  Default = c("Depth").

- reef_id_col:

  character or integer. Column containing the single unique reef ID to
  be assigned as an identifier before outputting cluster allocations.
  Default = "UNIQUE_ID".

- habitat_clustering_function:

  function. A function that takes a dataframe of points for a single
  habitat type as the first argument and additional arguments, and
  returns a dataframe of input points with an additional column
  containing assigned `site_id` values. Additional arguments must
  include `px_per_cluster`, `habitat_col`, `x_col`, `y_col` and
  `additional_variable_cols`. Default options available include
  `reef_skater` and `constrained_hclust`.

- clustering_function_args:

  named list. A named list where elements of the list are arguments that
  will be passed on to `habitat_clustering_function`. Defaults to an
  empty list default arguments of `habitat_clustering_function` are
  used.

## Value

data.frame containing points that have a site ID allocated in `site_id`
column. Output data includes clustering execution time.
