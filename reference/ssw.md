# Internal helper to compute the sum of square distances within cluster nodes. Follows spdep function, implementing igraph methods. For additional information see [spdep::ssw](https://r-spatial.github.io/spdep/reference/ssw.html)

Internal helper to compute the sum of square distances within cluster
nodes. Follows spdep function, implementing igraph methods. For
additional information see
[spdep::ssw](https://r-spatial.github.io/spdep/reference/ssw.html)

## Usage

``` r
ssw(data, nodes, method = "euclidean", p = 2, cov = NULL, inverted = FALSE)
```
