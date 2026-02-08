# Perform post-processing steps on site polygons.

Perform post-processing steps on site polygons for a reef. Post-
processing applies to multipolygons (sites made up of smaller
non-continuous polygons) only. This process involves identifying the
largest single polygon and removing any polygons that are outside a
user-defined distance threshold from the largest polygon.
Post-processing also involves removing any site polygons that are too
small, based on a user defined minimum site area.

## Usage

``` r
site_postprocessing(reef_site_polygons, min_site_area)
```

## Arguments

- reef_site_polygons:

  data.frame. Contains a row for each unique site, for a target reef of
  interest.

- min_site_area:

  numeric. Minimum threshold for removing sites that are too small in
  their total site area. Must be in the same units returned by
  [`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html).

## Value

data.frame containing all site polygons for the target reef after post-
processing has taken place on undersized or multipolygon sites.
