# Create triangulated edges from geographic coordinates of pixels.

Take a dataframe of pixels `pixels` and triangulate the vertices. Then
extract the edges between pixels for clustering inputs.

## Usage

``` r
prepare_tri_edges(pixels)
```

## Arguments

- pixels:

  sf data.frame. Holds values for pixel geometries.

## Value

Matrix containing trianguated edge values between pixels.
