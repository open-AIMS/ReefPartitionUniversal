# Generate a raster template based on sf `vector_data` at a specific `pixel_resolution`.

Generate a raster template based on sf `vector_data` at a specific
`pixel_resolution`.

## Usage

``` r
generate_raster_template(vector_data, pixel_resolution = NA)
```

## Arguments

- vector_data:

  sf data.frame. Vector data to generate a raster template object.

- pixel_resolution:

  numeric. Resolution of raster cells to use in template creation.
  Default is NA. If pixel_resolution is NA then a default resolution of
  0.001 degrees or 100m will be used depending on the unit of
  `vector_data` coordinate reference system. This default resolution is
  used to prevent long execution times and over-use of RAM.

## Value

template raster object based on vector_data extent.
