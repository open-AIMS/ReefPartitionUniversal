# Changelog

## ReefPartitionUniversal 1.0.2

### Bug fixes

- Fixed a bug in
  [`prepare_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_mst.md)
  (internal helper
  [`neighborsDataFrame()`](https://open-aims.github.io/ReefPartitionUniversal/reference/neighborsDataFrame.md))
  where edge costs computed by
  [`spdep::nbcosts()`](https://r-spatial.github.io/spdep/reference/nbcosts.html)
  were being paired with the wrong edges, because
  [`neighborsDataFrame()`](https://open-aims.github.io/ReefPartitionUniversal/reference/neighborsDataFrame.md)’s
  [`merge()`](https://rspatial.github.io/terra/reference/merge.html)-based
  implementation silently reordered rows relative to `nbcosts()`’s
  natural per-node order. This corrupted MST edge weights whenever
  `mst_alpha > 0` (impact proportional to `mst_alpha`; `mst_alpha = 0`
  was unaffected). Affects
  [`prepare_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/prepare_mst.md),
  and transitively
  [`reef_skater()`](https://open-aims.github.io/ReefPartitionUniversal/reference/reef_skater.md),
  [`reef_skater_fast()`](https://open-aims.github.io/ReefPartitionUniversal/reference/reef_skater_fast.md),
  and
  [`constrained_hclust_mst()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust_mst.md)
  when called with `mst_alpha > 0`.
  [`constrained_hclust()`](https://open-aims.github.io/ReefPartitionUniversal/reference/constrained_hclust.md)
  (called directly) and
  [`hclust_geo()`](https://open-aims.github.io/ReefPartitionUniversal/reference/hclust_geo.md)
  were not affected.
