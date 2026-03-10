# Helper function to convert points in a single site into an sf poylgon.

Helper function to convert points in a single site into an sf poylgon.

## Usage

``` r
hex_to_polygons(x, h3_id_col = "id", site_id_col = "site_id")
```

## Arguments

- x:

  data.frame. Contains point values for the target site.

- h3_id_col:

  character or integer. Column containing the H3 ID values for each
  point to be collated into the polygon. Default = "id".

- site_id_col:

  charcater vector. Column containing the unique site ID for the target
  site contained in `x`. Default = "site_id".

## Value

sf data.frame containing site polygons created from points using
allocated `site_id_col` values.
