# Convert points for an entire reef that have been allocated site IDs into site polygons.

From a dataframe of points and point data, this function clusters points
into sites (within each habitat type), using a user defined clustering
method that considers both geographical data and additional extracted
data (such as depth). **cells**\_to_polygons refers to points that have
been extracted using H3 methods. This method of polygon creation uses H3
methods to polygonise covered areas.

## Usage

``` r
cells_to_polygons(
  clustered_points,
  site_id_col = "site_id",
  reef_cols_to_keep = c("clustering_time", "UNIQUE_ID")
)
```

## Arguments

- clustered_points:

  data.frame. Contains a row for each point and cluster allocations.

- site_id_col:

  character or integer. Column containing site allocations for points.
  Default = "site_id".

- reef_cols_to_keep:

  character vector. Columns containing unique values per reef that can
  be allocated to the collated site polygons to conserve IDs and other
  reef level information. Examples include reef level mean depth, or
  reef distance to coastline. Default = c("clustering_time",
  "UNIQUE_ID").

## Value

sf data.frame containing site polygons created from points using
allocated `site_id_col` values.
