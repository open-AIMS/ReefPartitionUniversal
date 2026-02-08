# Convert pixels for an entire reef that have been allocated site IDs into site polygons.

From a dataframe of pixels and pixel data, this function clusters pixels
into sites (within each habitat type), using a user defined clustering
method that considers both geographical data and additional extracted
data (such as depth).

## Usage

``` r
clustered_pixels_to_polygons(
  clustered_pixels,
  site_id_col = "site_id",
  reef_cols_to_keep = c("clustering_time", "UNIQUE_ID")
)
```

## Arguments

- clustered_pixels:

  data.frame. Contains a row for each pixel and cluster allocations.

- site_id_col:

  character or integer. Column containing site allocations for pixels.
  Default = "site_id".

- reef_cols_to_keep:

  character vector. Columns containing unique values per reef that can
  be allocated to the collated site polygons to conserve IDs and other
  reef level information. Examples include reef level mean depth, or
  reef distance to coastline. Default = c("clustering_time",
  "UNIQUE_ID").

## Value

sf data.frame containing site polygons created from pixels using
allocated `site_id_col` values.
