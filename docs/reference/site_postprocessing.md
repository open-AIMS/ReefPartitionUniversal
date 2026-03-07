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
site_postprocessing(
  reef_site_polygons,
  min_site_area = 50 * 307,
  max_distance = 100
)
```

## Arguments

- reef_site_polygons:

  data.frame. Contains a row for each unique site, for a target reef of
  interest.

- min_site_area:

  numeric. Minimum threshold for removing sites that are too small in
  their total site area. This value should be smaller than the site_area
  used in other parts of the workflow as it is intended as an absolute
  minimum threshold. Must be in the same units returned by
  [`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html).
  Default value = 50 \* 307, where 307 is the area (in m^2) of a H3 cell
  with resolution of 12 and 50 is the minimum number of hexagons per
  site.

## Value

data.frame containing all site polygons for the target reef after post-
processing has taken place on undersized or multipolygon sites.
