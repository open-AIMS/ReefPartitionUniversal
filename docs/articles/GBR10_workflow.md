# Partitioning workflow using GBRMPA data

This vignette sets out and example reef partitioning workflow using data
available from GBRMPA for the Great Barrier Reef, Australia. The data
intended for use include the canonical-reefs geopackage which defines
reef vector outlines, GBR10 Geomorphic raster which contain benthic
habitat data for the GBR, and GBR10 Bathymetry raster which contain
bathymetry data for the GBR.

While this example vignette is intended and contains context notes and
arguments for the GBR, this approach is flexible and can be used on any
raster and vector data beyond habitat, bathymetry and reef examples.

## Required packages

``` r

library(ReefPartitionUniversal)
library(terra)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(exactextractr)
```

## Loading and formatting required data

GBRMPA GBR10 raster data is downloaded in UTM Projected Coordinate
Reference Systems. ReefPartitionUniversal requires that all input data
are in the same CRS before starting. To match the CRS we reproject
`reef_polygons` from GDA2020 as reprojecting raster data is more
computationally intensive. If all input data is already in the same CRS
then this step is not required.

``` r

# Load reef outline vector data (`canonical-reefs` used here)
reef_polygons <- st_read("Path to canonical reefs .gpkg")

# Load bathymetry raster data from GBRMPA GBR10 dataset
bathymetry_raster <- rast(
  "Path to Mackay - Capricorn GBR10 bathymetry raster .tif"
)

# Load geomorphic habitat raster data from GBRMPA GBR10 dataset
habitat_raster <- rast(
  "Path to Mackay - Capricorn GBR10 geomorphic habitat raster .tif"
)

# Must ensure all input data are in the same CRS
reef_polygons <- st_make_valid(st_transform(
  reef_polygons,
  crs = sf::st_crs(habitat_raster)
))
```

## Define arguments for workflow

The following code defines arguments for use in our example workflow
with One Tree Island Reef. This includes defining the target habitat
types from `habitat_raster`, defining the desired H3 cell resolution for
pixel extraction, and the desired site size for clustering.

``` r

set.seed(123)

# Define analysis parameters for example:
# Pixel values that are used to extract target habitats from
# `habitat_raster`
habitat_categories <- c(15, 22, 14, 21)
# 15: reef crest, 22: reef slope, 14: outer reef flat, 21: sheltered reef slope

# Define hexagon resolution and unit for outputting hexagon area
# (same as function defaults)
hex_resolution <- 12
unit <- "m2"

# Dataframe containing the H3 cell sizes for each resolution
# (from https://h3geo.org/docs/core-library/restable/#average-area-in-m2).
hex_size <- data.frame(
  res = c(7:15),
  size = c(5161293, 737327, 105332, 15047, 2149, 307.09, 43.87, 6.267, 0.895)
)

# Define the desired site size in terms of spatial area
# and the number of H3 cells per site
site_size <- 250 * 250 # set size size for our example to 625,000m~2~
n_pixels <- site_size / hex_size[hex_size$res == hex_resolution, ]$size

# Select target reef outline. For this example we use
# One Tree Island Reef from the Mackay - Capricorn region
OTIR_ID <- "23055101104" # One Tree Island Reef
target_reef <- reef_polygons[reef_polygons$UNIQUE_ID == OTIR_ID, ]
```

## Extracting pixel data for the target reef

Once the input data and arguments have been set up, we can extract pixel
data for One Tree Island Reef using our `target_reef` outline polygon.
This process involves identifying selected pixels from the habitat
raster object, converting pixels to H3 hexagon cells and extracting
bathymetry data.

``` r

pixel_data <- extract_pixel_points(
  reef_polygon = target_reef,
  habitat_raster = habitat_raster,
  add_var_raster = bathymetry_raster,
  habitat_categories = habitat_categories
)

# We must also attach a reef ID to the dataframe and
# remove any pixels with invalid depth data
pixel_data$UNIQUE_ID <- OTIR_ID
pixel_data <- pixel_data[!is.na(pixel_data$depth), ]

head(pixel_data)
```

## Clustering habitat pixels based on distance and depth

Data for each required pixel have been extracted, and we can now cluster
pixels based on their geographic distance and depth. Using the default
arguments for
[`cluster_reef_pixels()`](https://open-aims.github.io/ReefPartitionUniversal/reference/cluster_reef_pixels.md)
clusters pixels within each habitat type using a Minimum Spanning Tree
and
[`adespatial::constr.hclust`](http://adeverse.github.io/adespatial/reference/constr.hclust.md)
clustering algorithm. The returned dataframe contains a row for each
pixel and an additional column containing the clustered `site_id`.

``` r

mst_hclust_pixels <- cluster_reef_pixels(pixel_data, n_pixels = n_pixels)
```

## Creating site polygons

Once pixels for the reef have been assigned site IDs we can collate them
into site polygons and use post-processing to separate site areas that
contain large distances into smaller site IDs.

``` r

# Collate H3 cells that are assigned site IDs into polygons
mst_hclust_sites <- clustered_pixels_to_polygons(mst_hclust_pixels)

# Site postprocessing with a minimum number of pixels per site of 50
processed_sites <- site_postprocessing(mst_hclust_sites, min_site_area = 50)
```

## Mapping outputs using ggplot2

``` r

# Reorder site ID labels to improve readability
sampled_ids <- sample(levels(processed_sites$site_id))
processed_sites$sampled_id <- factor(
  processed_sites$site_id,
  levels = sampled_ids
)

# Plot site polygons coloured by IDs (colours are repeated)
id_map <- ggplot() +
  geom_sf(data = processed_sites, aes(fill = sampled_id)) +
  theme(legend.position = "none") # Remove legend due to too many site ID labels

# Re-extract depth data for site polygons for plotting
processed_sites$depth <- abs(exact_extract(
  bathymetry_raster,
  processed_sites,
  "mean"
))

# Plot sites coloured by mean depth
depth_map <- ggplot() +
  geom_sf(data = processed_sites, aes(fill = depth)) +
  scale_fill_fermenter(
    palette = "greens",
    direction = 1,
    breaks = c(2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20)
  )
```
