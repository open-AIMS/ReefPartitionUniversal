# Cluster pixels together using the adespatial::constr.hclust algorithm.

Take a dataframe of pixels containing `additional_variable_cols` values
and edges between pixels in the dataframe and cluster using the
constr.hclust algorithm. This clustering is performed bottom-up based on
a combined distance matrix of geographic and `additional_variable_cols`
distances Only pixels connected by `edges` are able to cluster together.
For additional information see **insert citation/link**. If pixels
contains over 30,000 observations then interpolation will be used to
reduce RAM usage. This process randomly samples 30,000 for clustering
and assigns clusters to the remaining pixels using nearest neighbour
interpolation.

## Usage

``` r
constrained_hclust(
  pixels,
  edges,
  x_col = "X_standard",
  y_col = "Y_standard",
  additional_variable_cols = c("depth_standard"),
  id_col = "UNIQUE_ID",
  habitat_col = "habitat",
  distance_method = "manhattan",
  distance_alpha = 0.5,
  beta = -1,
  n_pixels = 204,
  n_clust = (round(nrow(pixels)/n_pixels)),
  method = "ward.D2"
)
```

## Arguments

- pixels:

  data.frame. Contains values for X and Y coordinates, as well as
  `additional_variable_cols`.

- edges:

  matrix. Matrix containing edges between pixels in `pixels`.

- x_col:

  character. Name of the column holding X coordinates. Default =
  "X_standard".

- y_col:

  character. Name of the column holding Y coordinates. Default =
  "Y_standard".

- additional_variable_cols:

  character vector. Names of additional columns to contribute to the
  distance matrix. Default = c("depth_standard").

- id_col:

  character. Column holding ID value for the target reef (attached to
  the site_id values on output). Default = "UNIQUE_ID".

- habitat_col:

  character. Column holding unique habitat values (attached to `id_col`
  value and site_id values on output). Default = "habitat".

- distance_method:

  character. Distance matrix creation method. Default = "manhattan" (see
  dist()).

- distance_alpha:

  float numeric. Weighting applied to `additional_variable_cols`
  distance matrix when combining with geographic distances. (1 - alpha)
  weighting is applied to the geographic distance matrix. Default = 0.5
  (symmetric weighting).

- beta:

  float numeric. Beta parameter used by adespatial::constr.hclust.
  Parameter value is only used if `method` == "flexible". Default = -1.

- n_pixels:

  integer numeric. Desired number of pixels in resulting clusters. Used
  to calculate n_clust (number of output clusters). Value only used in
  n_clust specification. Default = 204.

- n_clust:

  integer numeric. Number of clusters in result output. (Point to cut
  hierarchical clustering tree). Default = (round(nrow(pixels) /
  n_pixels)) (dividing habitat into clusters containing an average of
  200 pixels).

- method:

  character. Clustering method to be applied. See
  adespatial::constr.hclust() for more details. Default = "ward.D2".

## Value

data.frame of pixels with allocated site_ids based on cluster outputs.
`site_id` values are a combination of the `id_col` value, `habitat_col`
value and the cluster allocation.
