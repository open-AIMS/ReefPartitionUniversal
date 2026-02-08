# Package index

## All functions

- [`clustered_pixels_to_polygons()`](https://open-aims.github.io/ReefPartitionUniversal/reference/clustered_pixels_to_polygons.md)
  : Convert pixels for an entire reef that have been allocated site IDs
  into site polygons.

- [`cluster_reef_pixels()`](https://open-aims.github.io/ReefPartitionUniversal/reference/cluster_reef_pixels.md)
  : Cluster pixels into sites based on geographical attributes and
  additional extracted pixel values.

- [`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
  : Cluster pixels together using the adespatial::constr.hclust
  algorithm.

- [`constrained_hclust_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust_mst.md)
  : Default habitat clustering function using adespatial::constr.hclust.

- [`extract_pixel_points()`](https://open-aims.github.io/ReefPartitionUniversal/reference/extract_pixel_points.md)
  : Extract pixel data from selected habitats over a target reef.

- [`hex_to_polygons()`](https://open-aims.github.io/ReefPartitionUniversal/reference/hex_to_polygons.md)
  : Helper function to convert pixels in a single site into an sf
  poylgon.

- [`neighborsDataFrame()`](https://open-aims.github.io/ReefPartitionUniversal/reference/neighborsDataFrame.md)
  :

  Helper function taken from package `expp` on 2025-01-20 that converts
  an spdep::nb neighbors object into a dataframe with columns `id` and
  `id_neigh`. Function copied from `expp` package to avoid expp
  dependency as this package is no longer maintained.

- [`prepare_knn_edges()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_knn_edges.md)
  : Create k-nearest-neighbour edges for clustering inputs.

- [`prepare_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_mst.md)
  : Create a minimum spanning tree from geographic coordinates of pixels
  and extracted data.

- [`prepare_tri_edges()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_tri_edges.md)
  : Create triangulated edges from geographic coordinates of pixels.

- [`reef_skater()`](https://open-aims.github.io/ReefPartitionUniversal/reference/reef_skater.md)
  : Cluster pixels together using the spdep::skater algorithm.

- [`site_postprocessing()`](https://open-aims.github.io/ReefPartitionUniversal/reference/site_postprocessing.md)
  : Perform post-processing steps on site polygons.
