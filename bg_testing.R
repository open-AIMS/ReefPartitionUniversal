#### Setup and run results for Partition paper

# List of packages to check and install if necessary
package_list <- c("sf","future.apply","modeest","rlang","readxl","R.matlab", "remotes","tidyverse", "raster","h3lib","stars","sp",
                  "stringr","RColorBrewer","viridisLite","viridis","sfheaders","gridExtra","tictoc",
                  "exactextractr","terra","FNN","readODS","doParallel","foreach","rmarkdown","viridis","ggplot2","h3")


# Source required scripts



# This function installs and loads the packages listed in package_list
#install_and_load_package(package_list)
lapply(package_list, require, character.only = TRUE)


##############################################
#1. Setup inputs for polygons for OneTree
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

bathymetric_file = c("LizardClusterBathy.tif") #bathy file name
bathymetric_path <- "../../reef_partitioning/Data/required_data/" 

bathymetric_file = c("MC_wg84.tif") #bathy file name
bathymetric_path <- "../../reef_partitioning/Data/required_data/bathy/Mackay-Capricorn/" 

reef_geomorphic_path <- "../../reef_partitioning/Data/required_data/" 

# reef_geomorphic_file <- "geomorphic 1.tiff"
# reef_geomorphic_file <- "output_file.tif"
reef_geomorphic_file <- "GBR10 GBRMP Geomorphic.tif"

full_geo_file_path = file.path(reef_geomorphic_path, reef_geomorphic_file)

# Hexagon resolution and unit for area calculation
resolution <- 12
unit <- "km2"
reshex <- 12


geo_zone_names =list(
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

min_polygon_size =20 # Filter threshold for minimum area in hexagons (default: 50 hexagons ≈ 15,350m² at res 12)

# Reef shape file path
reef_outline_file <- "../../reef_partitioning/Data/required_data/GBR_features_GDA94_download/"
reef_outline_file <- "../rrap_canonical_outlines.gpkg"

bathymetry_data <- terra::rast(file.path(bathymetric_path, bathymetric_file[1]))
bathy_box <- st_as_sfc(st_bbox(bathymetry_data))

reefs <- st_read(reef_outline_file)
reefs <- st_transform(reefs, crs=st_crs(bathy_box))
within <- sapply(reefs$geometry, function(x) st_within(st_make_valid(x), bathy_box))
within <- which(within == 1)

within_ids <- reefs[within, ]$UNIQUE_ID
region_IDs <- within_ids

# specify which reef zones are to be included e.g. #reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list_number = c(15, 22, 14, 21) #reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list = c("Reef Crest","Reef Slope","Outer Reef Flat","Sheltered Reef Slope")
geozone_list = c("Reef")
# geozone_list <- c(1, 2, 3, 4, 5, 7, 8, 9, 12) old list, commented 1/12
geozone_list = c(15, 22, 14, 21) # copied 1/12

# Define the geo_zone_names to be mapped dynamically
geo_zone_names <- NULL #default (will keep as numbers)


long_duration_reef <- 6


#######################
#2. Setup variations
######################


# Parameters for site polygon creation
site_size_vec <- c(100*100,250*250,500*500)

# num_cores <- parallel::detectCores(logical = FALSE) - 2L
# spdep::set.coresOption(num_cores)
# spdep::set.mcOption(FALSE)

# # 1. Create the cluster using the number of cores set in Step 1
# cl <- parallel::makeCluster(spdep::get.coresOption()) 

# # 2. Tell spdep to use this specific cluster object
# spdep::set.ClusterOption(cl)

#####################
#3. Create polygons
#####################

# Set a global seed for reproducibility
set.seed(123)
create_polygon<-TRUE
Save_name<-cluster_properties[[1]]$name

source("01_Run_CreateSitePolygons.R")
source("AddSpatialtoSitePolygons_functions.R")

test_slope_id = "17032100104"
shelter_slope_id = "21128100104"
ACRS_example_reef = "23055101104"

for (site_size in site_size_vec){
  cluster_properties[[1]]$name<-paste0(Save_name,site_size)
  
  sites<-create_site_polygons(output_directory, cluster_properties, bathymetric_file,
                       bathymetric_path,region_IDs,reef_outline_file,reef_geomorphic_path,
                       reef_geomorphic_file,site_size,reshex,resolution,unit,
                       geozone_list,geo_zone_names,min_polygon_size)
  sites_plus <- AddDepth(sites, bathy_file) 
  saveRDS(object = sites_plus, file = paste0(output_directory, "/dataframe_", cluster_properties[[1]]$name,".Rdata"))
  print(site_size)
}

source("~/GitHub/C_scape_input/01_step4_CreateSpatialFiles/01_Run_CreateSitePolygonsOld.R")

for (site_size in site_size_vec){
  cluster_properties[[1]]$name<-paste0(Save_name,site_size,"_noDepth2")
  
  sites<-create_site_polygons(output_directory, cluster_properties, bathymetric_file,
                              bathymetric_path,region_IDs,reef_outline_file,reef_geomorphic_path,
                              reef_geomorphic_file,site_size,reshex,resolution,unit,
                              geozone_list,geo_zone_names,min_polygon_size)
  sites_plus <- AddDepth(sites, bathy_file) 
  saveRDS(object = sites_plus, file = paste0(output_directory, "/dataframe_", cluster_properties[[1]]$name,".Rdata"))
  print(site_size)
}

start_time <- Sys.time()
test = lapply(hab.pts,
                     FUN = site_clust3, saveDirectory=saveDirectory, site_size=site_size)
end <- Sys.time()

parallel_time = end - start_time

# spdep::set.ClusterOption(NULL)
# parallel::stopCluster(cl)
