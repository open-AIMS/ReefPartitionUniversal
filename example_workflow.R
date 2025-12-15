# These packages will likely go into our package's dependencies when the collated package is working
library(sf)
library(terra)
library(h3)
library(sfnetworks)
library(igraph)
library(spdep)
library(adespatial)
library(tidyverse)

# library(ReefPartition) # after package has been built
# As package is not ready for building we need to source each function
source("./R/pixel_extraction.R")
source("./R/cluster_reef_pixels.R")
source("./R/clustering_algorithms/constrained_hclust.R")
source("./R/clustering_algorithms/prepare_cluster_edges.R")
source("./R/clustering_algorithms/reef_skater.R")
source("./R/site_postprocessing.R")

# Set seed for reproducibility
set.seed(123)

ID <- "23055101104" # One Tree Island Reef

reef_polygons <- st_read("../canonical-reefs/output/rrap_canonical_2025-01-10-T13-28-06.gpkg")

bathy_raster <- rast("../../reef_partitioning/Data/required_data/bathy/Mackay-Capricorn/MC_wg84.tif")
habitat_raster <- rast("../../reef_partitioning/Data/required_data/GBR10 GBRMP Geomorphic.tif")

# Must ensure all input data are in the same CRS
reef_polygons <- st_transform(reef_polygons, crs = st_crs(habitat_raster)) %>%
    st_make_valid()

habitat_categories <- c(15, 22, 14, 21) # reef crest, reef slope, outer reef flat, sheltered reef slope

# Define hexagon resolution and unit for outputting hexagon area (same as function defaults)
hex_resolution <- 12
unit <- "km2"

# Extract pixel data for target reef - OTI reef
target_reef <- reef_polygons[reef_polygons$UNIQUE_ID == ID, ]
pixel_data <- extract_pixel_points(
    target_reef,
    habitat_raster,
    bathy_raster,
    habitat_categories,
    hex_resolution = hex_resolution,
    unit = unit,
    additional_variable_name = "depth",
    output_epsg = 3112,
    resample_method = "bilinear"
)
pixel_data$UNIQUE_ID <- target_reef$UNIQUE_ID

# Test approach with MST constr.hclust
mst_hclust_pixels <- cluster_reef_pixels(
    pixel_data,
    habitat_clustering_function = function(x) {
        mst <- prepare_mst(x)
        constrained_hclust(x, as_edgelist(mst))
    }
)
mst_hclust_sites <- clustered_pixels_to_polygons(mst_hclust_pixels)

# Test approach with triangulated constr.hclust
tri_hclust_pixels <- cluster_reef_pixels(
    pixel_data,
    habitat_clustering_function = function(x) {
        tri_edges <- prepare_tri_edges(x)
        constrained_hclust(x, tri_edges)
    }
)
tri_hclust_sites <- clustered_pixels_to_polygons(tri_hclust_pixels)

# Test approach with high weight constr.hclust
high_weight_hclust_pixels <- cluster_reef_pixels(
    pixel_data,
    habitat_clustering_function = function(x) {
        tri_edges <- prepare_tri_edges(x)
        constrained_hclust(x, tri_edges, alpha = 0.7)
    }
)
high_weight_hclust_sites <- clustered_pixels_to_polygons(high_weight_hclust_pixels)

# Test approach with skater (as in previous functions)
skater_pixels <- cluster_reef_pixels(
    pixel_data,
    habitat_clustering_function = function(x) reef_skater(x)
)
saveRDS(skater_pixels, "OTIReef_skater.rds")
skater_sites <- clustered_pixels_to_polygons(skater_pixels)