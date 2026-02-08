# Cluster pixels into sites based on geographical attributes and additional extracted pixel values.

From a dataframe of pixels and pixel data, this function clusters pixels
into sites (within each habitat type), using a user defined clustering
method that considers both geographical data and additional extracted
data (such as depth).

## Usage

``` r
cluster_reef_pixels(
  pixels,
  habitat_col = "habitat",
  x_col = "X",
  y_col = "Y",
  additional_variable_cols = c("depth"),
  reef_id_col = "UNIQUE_ID",
  habitat_clustering_function = constrained_hclust_mst,
  ...
)
```

## Arguments

- pixels:

  data.frame. Contains a row for each pixel and values for each variable
  to be input into `habitat_clustering_function`.

- habitat_col:

  character or integer. Column containing the categorical habitat
  allocations of pixels. Default = "habitat".

- x_col:

  character or integer. Column containing the continuous geographical X
  values for pixels. Default = "X".

- y_col:

  character or integer. Column containing the continuous geographical Y
  values for pixels. Default = "Y".

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

  function. A function that takes a dataframe of pixels for a single
  habitat type as the first argument and additional arguments, and
  returns a dataframe of input pixels with an additional column
  containing assigned `site_id` values. Additional arguments must
  include `px_per_cluster`, `habitat_col`, `x_col`, `y_col` and
  `additional_variable_cols`. Default options available include
  `reef_skater` and `constrained_hclust`.

- px_per_cluster:

  integer. The number of pixels to be allocated into each cluster using
  `habitat_clustering_function`. If `habitat_clustering_function`
  defines k-clusters, this must be `round(nrow(x) / px_per_cluster)`.
  Default = 200 pixels.

## Value

data.frame containing pixels that have a site ID allocated in `site_id`
column. Output data includes clustering execution time.
