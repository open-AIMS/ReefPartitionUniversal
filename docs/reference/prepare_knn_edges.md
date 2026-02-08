# Create k-nearest-neighbour edges for clustering inputs.

Take a dataframe of pixels `pixels` and create k-nearest-neighbour
graphs between pixels based on geographic distances.

## Usage

``` r
prepare_knn_edges(pixels, k = 7)
```

## Arguments

- pixels:

  sf data.frame. Holds values for pixel geometries.

- k:

  integer. K-nearest-neighbours

## Value

Matrix containing k-n-n edge values between pixels.
