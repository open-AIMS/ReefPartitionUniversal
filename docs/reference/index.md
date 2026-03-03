# Package index

## All functions

- [`cells_to_polygons()`](https://open-aims.github.io/ReefPartitionUniversal/reference/cells_to_polygons.md)
  : Convert points for an entire reef that have been allocated site IDs
  into site polygons.

- [`cluster_reef_points`](https://open-aims.github.io/ReefPartitionUniversal/reference/cluster_reef_points.md)
  : Cluster points into sites based on geographical attributes and
  additional extracted point values.

- [`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
  : Cluster points together using the adespatial::constr.hclust
  algorithm.

- [`constrained_hclust_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust_mst.md)
  : Default habitat clustering function using adespatial::constr.hclust.

- [`extract_point_cells()`](https://open-aims.github.io/ReefPartitionUniversal/reference/extract_point_cells.md)
  : Extract pixel data from selected habitats over a target reef.

- [`extract_point_pixels()`](https://open-aims.github.io/ReefPartitionUniversal/reference/extract_point_pixels.md)
  : Extract pixel data from selected habitats over a target reef.

- [`fill_na_nearest()`](https://open-aims.github.io/ReefPartitionUniversal/reference/fill_na_nearest.md)
  : Internal helper function that fills NA values from columns with
  values from the nearest neighbouring points.

- [`generate_raster_template()`](https://open-aims.github.io/ReefPartitionUniversal/reference/generate_raster_template.md)
  :

  Generate a raster template based on sf `vector_data` at a specific
  `pixel_resolution`.

- [`hex_to_polygons()`](https://open-aims.github.io/ReefPartitionUniversal/reference/hex_to_polygons.md)
  : Helper function to convert points in a single site into an sf
  poylgon.

- [`input_check()`](https://open-aims.github.io/ReefPartitionUniversal/reference/input_check.md)
  : target reef.

- [`neighborsDataFrame()`](https://open-aims.github.io/ReefPartitionUniversal/reference/neighborsDataFrame.md)
  :

  Helper function taken from package `expp` on 2025-01-20 that converts
  an spdep::nb neighbors object into a dataframe with columns `id` and
  `id_neigh`. Function copied from `expp` package to avoid expp
  dependency as this package is no longer maintained.

- [`pixels_to_polygons()`](https://open-aims.github.io/ReefPartitionUniversal/reference/pixels_to_polygons.md)
  : Convert clustered points into collated site polygons

- [`prepare_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_mst.md)
  : Create a minimum spanning tree from geographic coordinates of points
  and extracted data.

- [`reef_skater()`](https://open-aims.github.io/ReefPartitionUniversal/reference/reef_skater.md)
  : Cluster points together using the spdep::skater algorithm.

- [`reef_skater_fast()`](https://open-aims.github.io/ReefPartitionUniversal/reference/reef_skater_fast.md)
  : Cluster points together using an optimized SKATER algorithm.

- [`site_postprocessing()`](https://open-aims.github.io/ReefPartitionUniversal/reference/site_postprocessing.md)
  : Perform post-processing steps on site polygons.

- [`skater_igraph()`](https://open-aims.github.io/ReefPartitionUniversal/reference/skater_igraph.md)
  : An approximate Skater algorithm implemented using igraph methods.

- [`ssw()`](https://open-aims.github.io/ReefPartitionUniversal/reference/ssw.md)
  : Internal helper to compute the sum of square distances within
  cluster nodes. Follows spdep function, implementing igraph methods.

- [`vector_to_raster()`](https://open-aims.github.io/ReefPartitionUniversal/reference/vector_to_raster.md)
  : Convert vector data for a reef(s) to raster data for pixel
  extraction.
