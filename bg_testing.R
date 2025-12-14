#### Setup and run results for Partition paper

# List of packages to check and install if necessary
package_list <- c(
  "sf", "future.apply", "modeest", "rlang", "readxl", "R.matlab", "remotes", "tidyverse", "raster", "h3lib", "stars", "sp",
  "stringr", "RColorBrewer", "viridisLite", "viridis", "sfheaders", "gridExtra", "tictoc",
  "exactextractr", "terra", "FNN", "readODS", "doParallel", "foreach", "rmarkdown", "viridis", "ggplot2", "h3"
)


# Source required scripts



# This function installs and loads the packages listed in package_list
# install_and_load_package(package_list)
lapply(package_list, require, character.only = TRUE)


##############################################
# 1. Setup inputs for polygons for OneTree
##############################################



# Output directory for saving results
output_directory <- "./outputs"

# Flags for creating or adding to site polygons
# to start with just create the polygons and worry about adding the additional information afterwards
create_polygon <- TRUE
add_site_to_polygon <- FALSE

cluster_properties <- list(
  list(name = "Test", lat = c(-19, -20), long = c(158, 159), use_exisitng_folder = FALSE, existing_cluster_folder_name = "", use_past_polygon = FALSE)
)

bathymetric_file <- c("LizardClusterBathy.tif") # bathy file name
bathymetric_path <- "../../reef_partitioning/Data/required_data/"

bathymetric_file <- c("MC_wg84.tif") # bathy file name
bathymetric_path <- "../../reef_partitioning/Data/required_data/bathy/Mackay-Capricorn/"

reef_geomorphic_path <- "../../reef_partitioning/Data/required_data/"

# reef_geomorphic_file <- "geomorphic 1.tiff"
# reef_geomorphic_file <- "output_file.tif"
reef_geomorphic_file <- "GBR10 GBRMP Geomorphic.tif"

full_geo_file_path <- file.path(reef_geomorphic_path, reef_geomorphic_file)

# Hexagon resolution and unit for area calculation
resolution <- 12
unit <- "km2"
reshex <- 12


geo_zone_names <- list(
  c(22, "Slope"),
  c(15, "Crest"),
  c(14, "Outer Flat"),
  c(13, "Inner Flat"),
  c(24, "Back Reef"),
  c(25, "Patch"),
  c(21, "Sheltered Slope"),
  c(2, "Deep"),
  c(11, "Shallow Lagoon"),
  c(12, "Deep Lagoon"),
  c(23, "Plateau")
)



# List the unique reef IDs you want to create site polygons for after examining the GBR_map.
region_IDs <- c(23055101104, 21118100104, 14137100104)
region_IDs <- region_IDs[3]

min_polygon_size <- 20 # Filter threshold for minimum area in hexagons (default: 50 hexagons ≈ 15,350m² at res 12)

# Reef shape file path
reef_outline_file <- "../../reef_partitioning/Data/required_data/GBR_features_GDA94_download/"
reef_outline_file <- "../rrap_canonical_outlines.gpkg"

bathymetry_data <- terra::rast(file.path(bathymetric_path, bathymetric_file[1]))
bathy_box <- st_as_sfc(st_bbox(bathymetry_data))

reefs <- st_read(reef_outline_file)
reefs <- st_transform(reefs, crs = st_crs(bathy_box))
within <- sapply(reefs$geometry, function(x) st_within(st_make_valid(x), bathy_box))
within <- which(within == 1)

within_ids <- reefs[within, ]$UNIQUE_ID
region_IDs <- within_ids

# specify which reef zones are to be included e.g. #reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list_number <- c(15, 22, 14, 21) # reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list <- c("Reef Crest", "Reef Slope", "Outer Reef Flat", "Sheltered Reef Slope")
geozone_list <- c("Reef")
# geozone_list <- c(1, 2, 3, 4, 5, 7, 8, 9, 12) old list, commented 1/12
geozone_list <- c(15, 22, 14, 21) # copied 1/12

# Define the geo_zone_names to be mapped dynamically
geo_zone_names <- NULL # default (will keep as numbers)


long_duration_reef <- 6


#######################
# 2. Setup variations
######################


# Parameters for site polygon creation
site_size_vec <- c(100 * 100, 250 * 250, 500 * 500)

# num_cores <- parallel::detectCores(logical = FALSE) - 2L
# spdep::set.coresOption(num_cores)
# spdep::set.mcOption(FALSE)

# # 1. Create the cluster using the number of cores set in Step 1
# cl <- parallel::makeCluster(spdep::get.coresOption())

# # 2. Tell spdep to use this specific cluster object
# spdep::set.ClusterOption(cl)

#####################
# 3. Create polygons
#####################

# Set a global seed for reproducibility
set.seed(123)
create_polygon <- TRUE
Save_name <- cluster_properties[[1]]$name

source("01_Run_CreateSitePolygons.R")
source("AddSpatialtoSitePolygons_functions.R")

test_slope_id <- "17032100104"
shelter_slope_id <- "21128100104"
ACRS_example_reef <- "23055101104"

for (site_size in site_size_vec) {
  cluster_properties[[1]]$name <- paste0(Save_name, site_size)

  sites <- create_site_polygons(
    output_directory, cluster_properties, bathymetric_file,
    bathymetric_path, region_IDs, reef_outline_file, reef_geomorphic_path,
    reef_geomorphic_file, site_size, reshex, resolution, unit,
    geozone_list, geo_zone_names, min_polygon_size
  )
  sites_plus <- AddDepth(sites, bathy_file)
  saveRDS(object = sites_plus, file = paste0(output_directory, "/dataframe_", cluster_properties[[1]]$name, ".Rdata"))
  print(site_size)
}

source("~/GitHub/C_scape_input/01_step4_CreateSpatialFiles/01_Run_CreateSitePolygonsOld.R")

for (site_size in site_size_vec) {
  cluster_properties[[1]]$name <- paste0(Save_name, site_size, "_noDepth2")

  sites <- create_site_polygons(
    output_directory, cluster_properties, bathymetric_file,
    bathymetric_path, region_IDs, reef_outline_file, reef_geomorphic_path,
    reef_geomorphic_file, site_size, reshex, resolution, unit,
    geozone_list, geo_zone_names, min_polygon_size
  )
  sites_plus <- AddDepth(sites, bathy_file)
  saveRDS(object = sites_plus, file = paste0(output_directory, "/dataframe_", cluster_properties[[1]]$name, ".Rdata"))
  print(site_size)
}

start_time <- Sys.time()
test <- lapply(hab.pts,
  FUN = site_clust3, saveDirectory = saveDirectory, site_size = site_size
)
end <- Sys.time()

parallel_time <- end - start_time

# spdep::set.ClusterOption(NULL)
# parallel::stopCluster(cl)


#### Full test of constr.hclust() ####
region_IDs <- ACRS_example_reef

# Load the bathymetry file using terra
bathy_file <- file.path(paste(bathymetric_path, sep = "/"), bathymetric_file)
bathymetry_raster <- rast(bathy_file)
geomorphic_raster <- rast(full_geo_file_path)

hex_size <- data.frame(
  Res = c(7:15),
  Size = c(5161293, 737327, 105332, 15047, 2149, 307.09, 43.87, 6.267, 0.895)
)
max_cluster_size <- (1 / 3) # Reefs should have at least 3 clusters
reef_map <- sf::read_sf(reef_outline_file)

reef_areas <- reef_map[reef_map$UNIQUE_ID %in% region_IDs, ]
reef_areas$area <- NA
reef_areas$MaxCount <- NA
reef_areas$spdep_skater_time <- NA
reef_areas$pixel_preprocessing_time <- NA
for (id in reef_areas$UNIQUE_ID) {
  reef_area <- st_area(st_union(st_make_valid(reef_areas[reef_areas$UNIQUE_ID == id, ]$geometry)))
  reef_areas[reef_areas$UNIQUE_ID == id, ]$area <- reef_area
  reef_areas[reef_areas$UNIQUE_ID == id, ]$MaxCount <- round(reef_area * max_cluster_size / hex_size$Size[hex_size$Res == resolution])
}

hab <- list()
sites <- list()

source("CreateSitePolygons_functions.R")

for (ID in region_IDs) {
  index <- match(ID, region_IDs)

  # CreatePixels2 - this creates all the hexagonal pixels and assigns depth
  # hab[[which(region_IDs == ID)]] <- CreatePixels(
  #   reef_name = reef_Names[index],
  #   ROI = ROI[ROI$UNIQUE_ID == ID, ], reshex, site_size, bathy_file,
  #   full_geo_file_path, geozone_list, geo_zone_names, TRUE, resolution
  # )
  tic()
  hab[[which(region_IDs == ID)]] <- CreatePixels_mod(
    reef_name = "OTIR",
    ROI = st_make_valid(reef_map[reef_map$UNIQUE_ID == ID, ]), reshex, site_size, bathymetry_raster,
    geomorphic_raster, geozone_list, geo_zone_names, TRUE, resolution
  )
  toc(log = TRUE, quiet = TRUE)

  reef_areas[reef_areas$UNIQUE_ID == ID, ]$pixel_preprocessing_time <- tic.log()[[1]]
  tic.clearlog()

  print("Pixels created")
  sites_hab <- do.call(rbind, hab)
}

# Standardize X, Y, and Depth values ### BG Should this be happening within each reef, because that is the scale of pixel clustering?
sites_hab$X_standard <- scale(sites_hab$X)
sites_hab$Y_standard <- scale(sites_hab$Y)
sites_hab$Depth_standard <- scale(sites_hab$Depth)

sites_hab <- left_join(sites_hab, st_drop_geometry(reef_areas[, c("UNIQUE_ID", "MaxCount")]), by = "UNIQUE_ID")

alpha_values <- seq(0.1, 1, 0.1) # Test all alpha values from 0.1 to 1 (inclusive)

alpha_clusters <- lapply(alpha_values, function(x) {
  hab.pts <- sites_hab[sites_hab$UNIQUE_ID == ID, ]

  hab.pts <- dplyr::distinct(hab.pts) # anna changes here from unique() which was giving an error

  UNIQUE_ID <- unique(hab.pts$UNIQUE_ID)
  Reef <- unique(hab.pts$Reef)

  # scale variables
  hab.pts$X_standard <- scale(hab.pts$X)
  hab.pts$Y_standard <- scale(hab.pts$Y)
  hab.pts$Depth_standard <- scale(hab.pts$Depth)

  # this turns to a list
  hab.pts <- hab.pts %>%
    split(., hab.pts$habitat, drop = TRUE)

  ## Make Clusters
  hab.pts <- lapply(hab.pts,
    FUN = site_hrclust, saveDirectory = saveDirectory, alpha = x
  )
  hab.pts <- do.call(rbind, hab.pts)

  ## Make Sites
  # hab.pts$site_id <- as.factor(paste(Reef, hab.pts$habitat, hab.pts$site_id,sep="_"))
  hab.pts$site_id <- hab.pts$hclust_site_id
  sampled_levels <- sample(levels(hab.pts$site_id))
  hab.pts$sampled_site_id <- factor(hab.pts$site_id, levels = sampled_levels)

  ## Make Sites
  split_hab.pts <- hab.pts %>% split(., hab.pts$site_id)

  # use function 'group_hex' to group hexagons into polygon or multipolygon
  sites <- lapply(split_hab.pts, group_hex)

  sites <- do.call(rbind, sites)

  sites$sampled_site_id <- factor(sites$site_id, levels = sampled_levels)
  sites$Reef <- Reef
  sites$UNIQUE_ID <- UNIQUE_ID
  sites$alpha <- x
  hab.pts$alpha <- x

  return(list(sites = sites, hab.pts = hab.pts))
})

clustered_pixels <- do.call(rbind, lapply(alpha_clusters, function(x) x$hab.pts))
clustered_pixels$hexagons <- h3_to_geo_boundary_sf(clustered_pixels$h3_index)
clustered_pixels$depth <- abs(exact_extract(bathymetry_raster, clustered_pixels$hexagons, "mean"))

clustered_sites <- do.call(rbind, lapply(alpha_clusters, function(x) x$sites))
clustered_sites$depth <- abs(exact_extract(bathymetry_raster, clustered_sites, "mean"))

pixel_sd <- clustered_pixels %>%
  group_by(alpha, habitat, site_id) %>%
  summarise(depth_sd = sd(depth), .groups = "keep") %>%
  group_by(alpha, habitat) %>%
  summarise(mean_habitat_sd = mean(depth_sd), variance_habitat_sd = var(depth_sd))

pixel_sd$habitat <- as.character(pixel_sd$habitat)
pixel_sd <- pivot_longer(pixel_sd, c("mean_habitat_sd", "variance_habitat_sd"))

habitat_labels <- c(
  "15" = "reef crest",
  "22" = "reef_slope",
  "14" = "outer reef slope",
  "21" = "sheltered reef slope"
)

pixel_sd_plot <- ggplot() +
  geom_line(
    data = pixel_sd[pixel_sd$name == "mean_habitat_sd", ],
    aes(
      x = alpha,
      y = value,
      color = habitat
    )
  ) +
  ylab("Within site depth SD (metres)") +
  xlab("Weighting applied to depth in clustering") +
  scale_color_discrete(labels = habitat_labels)

full_reef_sites <- ggplot() +
  geom_sf(data = clustered_sites, aes(fill = sampled_site_id)) +
  facet_wrap(~alpha, labeller = function(variable, value) {
    paste("Depth weight", value)
  }) +
  theme(legend.position = "none")
full_reef_depth <- ggplot() +
  geom_sf(data = clustered_sites, aes(fill = depth)) +
  facet_wrap(~alpha, labeller = function(variable, value) {
    paste("Depth weight", value)
  }) +
  scale_fill_fermenter(palette = "Greens", direction = 1, breaks = c(2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20))

sub_clust_sites <- st_crop(clustered_sites, xmin = 152.06, xmax = 152.11, ymin = -23.480, ymax = -23.515)
sub_slope_sites <- ggplot() +
  geom_sf(data = sub_clust_sites[sub_clust_sites$habitat == 22, ], aes(fill = sampled_site_id)) +
  facet_wrap(~alpha, labeller = function(variable, value) {
    paste("Depth weight", value)
  }) +
  theme(legend.position = "none")
sub_slope_depth <- ggplot() +
  geom_sf(data = sub_clust_sites[sub_clust_sites$habitat == 22, ], aes(fill = depth)) +
  facet_wrap(~alpha, labeller = function(variable, value) {
    paste("Depth weight", value)
  }) +
  scale_fill_fermenter(palette = "Greens", direction = 1, breaks = c(2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20))
