# ReefPartitionUniversal 1.0.2

## Bug fixes

* Fixed a bug in `prepare_mst()` (internal helper `neighborsDataFrame()`)
  where edge costs computed by `spdep::nbcosts()` were being paired with
  the wrong edges, because `neighborsDataFrame()`'s `merge()`-based
  implementation silently reordered rows relative to `nbcosts()`'s natural
  per-node order. This corrupted MST edge weights whenever `mst_alpha > 0`
  (impact proportional to `mst_alpha`; `mst_alpha = 0` was unaffected).
  Affects `prepare_mst()`, and transitively `reef_skater()`,
  `reef_skater_fast()`, and `constrained_hclust_mst()` when called with
  `mst_alpha > 0`. `constrained_hclust()` (called directly) and
  `hclust_geo()` were not affected.
