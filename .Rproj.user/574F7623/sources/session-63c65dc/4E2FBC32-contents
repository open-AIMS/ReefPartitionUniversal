#### Setup and run results for Partition paper

setwd("C:/Users/vhaller/Documents/GitHub/ReefPartitionAllenAtlas")
#libraries

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
output_directory <- "C:/Users/vhaller/OneDrive - Australian Institute of Marine Science/DecisionSupport/BiodiversityReefPartition" 

# Flags for creating or adding to site polygons
# to start with just create the polygons and worry about adding the additional information afterwards
create_polygon <- TRUE
add_site_to_polygon <- FALSE

cluster_properties <- list(
  list(name = "Test", lat = c(-19, -20), long = c(158, 159), use_exisitng_folder = FALSE, existing_cluster_folder_name = "", use_past_polygon = FALSE)
)

bathymetric_file = c("coral_sea_example_bathy.tif") #bathy file name
bathymetric_path <- "C:/Users/vhaller/OneDrive - Australian Institute of Marine Science/DecisionSupport/BiodiversityReefPartition" 

reef_geomorphic_path <- "C:/Users/vhaller/OneDrive - Australian Institute of Marine Science/DecisionSupport/BiodiversityReefPartition" 

reef_geomorphic_file <- "geomorphic 1.tiff"
reef_geomorphic_file <- "output_file.tif"
reef_geomorphic_file <- "geomorph_gbr10_res.tif"

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
region_IDs <- c(23055101104)

min_polygon_size =20 # Filter threshold for minimum area in hexagons (default: 50 hexagons ≈ 15,350m² at res 12)


# Reef shape file path
reef_outline_file <- "C:/Users/vhaller/OneDrive - Australian Institute of Marine Science/DecisionSupport/BiodiversityReefPartition/reef_combined.shp" 

# specify which reef zones are to be included e.g. #reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list_number = c(15, 22, 14, 21) #reef crest, reef slope, outer reef flat, sheltered reef slope
geozone_list = c("Reef Crest","Reef Slope","Outer Reef Flat","Sheltered Reef Slope")
geozone_list = c("Reef")
geozone_list <- c(1, 2, 3, 4, 5, 7, 8, 9, 12)


# Define the geo_zone_names to be mapped dynamically
geo_zone_names <- NULL #default (will keep as numbers)




#######################
#2. Setup variations
######################


# Parameters for site polygon creation
site_size_vec <- c(100*100,250*250,500*500)



#####################
#3. Create polygons
#####################

# Set a global seed for reproducibility
set.seed(123)
create_polygon<-TRUE
Save_name<-cluster_properties[[1]]$name

source("01_Run_CreateSitePolygons.R")
source("AddSpatialtoSitePolygons_functions.R")

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


