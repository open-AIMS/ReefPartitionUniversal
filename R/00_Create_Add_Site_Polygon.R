
#' @title Create and Add Site Polygons
#'
#' @description This function either creates new site polygons or adds spatial data to existing site polygons, depending on the user’s request. It allows for flexibility in generating and updating spatial data based on connectivity, bathymetric, and other related datasets.
#'
#' @param connectivity_folder A character string specifying the directory path where the connectivity files are located.
#' @param conn_files A character string or vector of strings specifying the names of the connectivity files to be used.
#' @param cluster_properties An object containing properties related to reef clusters, which may be used in the creation or processing of site polygons.
#' @param bathymetric_file A character string specifying the name of the bathymetric file to be used.
#' @param bathymetric_path A character string specifying the directory path where the bathymetric file is located.
#' @param ub_files A character string or vector of strings specifying the names of the underlying bathymetric files to be used.
#' @param ub_files_path A character string specifying the directory path where the underlying bathymetric files are located.
#' @param previous_spatial_file A character string specifying the path to the previous spatial file, which may be used to build upon or modify existing spatial data.
#' @param rootdir_data A character string specifying the root directory path where data related to the project is stored.
#' @param output_directory A character string specifying the directory where output files should be saved.
#' @param site_polygons_rdata A character string specifying the file name for the RData file containing the site polygons.
#' @param reef_geomorphic_path A character string specifying the directory path where the benthic habitat raster file is located.
#' @param reef_geomorphic_file A character string specifying the name of the benthic habitat raster file (.tif) to be used for extracting habitat data.
#' @param gbr_zones_csv A character string specifying the path to the CSV file containing GBR zones information.
#' @param region_IDs A vector of IDs representing the Great Barrier Reef sites to be included in the analysis.
#' @param reef_outline_file A character string specifying the path to the shapefile for the Great Barrier Reef.
#' @param site_size A numeric value specifying the desired size of the site polygons.
#' @param reshex A character string specifying the resolution of the hexagons to be used in the spatial analysis.
#' @param resolution An integer specifying the resolution level for the hexagons in the H3 grid system.
#' @param geozone_list A vector list to specify to specify which reef zones are to be included 
#' @param min_polygon_size numeric value indicating the minimum area threshold (in number of hexagons).
#'                        Filters out any polygons smaller than this threshold.
#'                        Default is 50 hexagons (approximately 15,350m² at resolution 12).
#' @param geo_zone_names A list of numeric-value pairs, where each element is a vector of length 2. The first element of each vector is a numeric value (to check divisibility),and the second element is the corresponding category name (string) to assign.
#' @param unit A character string specifying the unit of measurement for the hexagon area (e.g., "m^2" for square meters).
#' @param create_polygon A logical value indicating whether to create new site polygons. Defaults to `FALSE`.
#' @param add_site_to_polygon A logical value indicating whether to add spatial data to existing site polygons. Defaults to `TRUE`.
#' @param create_spatial_files A logical value indicating whether to create new spatial files (e.g., shapefiles, RData, CSV). Defaults to `FALSE`.
#' @param apply_fix A logical value indicating whether to apply fixes to an existing spatial file (e.g., adjusting UNIQUE_ID or carrying capacity). Defaults to `FALSE`.
#' @param unique_id_mappings A named vector specifying the mappings of old UNIQUE_IDs to new ones. Use this to apply changes to UNIQUE_IDs in the spatial data. Defaults to `NULL`.
#' @param carrying_capacity_adjustments A named vector specifying the reef_siteid values and the corresponding adjustments to their carrying capacity (k). Defaults to `NULL`.
#' @param cluster_path A character string specifying the path to the folder where the existing cluster RData file is located. Used when applying fixes to an existing spatial file.
#' @param existing_cluster_file A character string specifying the name of the existing cluster RData file to which the fix will be applied (e.g., "LizardIslandCluster_allvars_dataframe.Rdata").
#' 
#' @details The function performs the following steps:
#' \itemize{
#'   \item If `create_polygon` is `TRUE`, it calls the script to create site polygons using the provided data.
#'   \item If `add_site_to_polygon` is `TRUE`, it calls the script to add spatial data to existing site polygons.
#' }
#'
#' @return Returns an updated spatial object, typically an `sf` object, that includes the newly created or updated site polygons.
#'
#' @examples
#' # Example usage of the Create_add_site_polygons function
#' Create_add_site_polygons(
#'   connectivity_folder = "path/to/connectivity",
#'   conn_files = "connectivity_file.csv",
#'   cluster_properties = cluster_properties_object,
#'   bathymetric_file = "bathymetry.tif",
#'   bathymetric_path = "path/to/bathymetry",
#'   ub_files = "ub_file.csv",
#'   ub_files_path = "path/to/ub_files",
#'   previous_spatial_file = "path/to/previous_spatial.RData",
#'   rootdir_data = "path/to/root_data",
#'   output_directory = "path/to/output",
#'   site_polygons_rdata = "site_polygons.RData",
#'   reefs_path = "path/to/reefs",
#'   reef_geomorphic_file = "benthic_habitat.tif",
#'   gbr_zones_csv = "GBR_zones.csv",
#'   region_IDs = c(1, 2, 3),
#'   reef_outline_file = "path/to/GBR_shapefile.shp",
#'   site_size = 100,
#'   reshex = "resolution_hex",
#'   resolution = 9,
#'   unit = "m^2",
#'   create_polygon = TRUE,
#'   add_site_to_polygon = TRUE
#' )
#' 
#' @authors  
#' Pascal Omondiagbe
Create_add_site_polygons<-function(connectivity_folder,conn_files, cluster_properties, bathymetric_file,bathymetric_path, ub_files, ub_files_path,previous_spatial_file,
                                   rootdir_data,output_directory,site_polygons_rdata,reef_geomorphic_path,
                                   reef_geomorphic_file,gbr_zones_csv,reef_outline_file,site_size,reshex,resolution,unit,
                                   geo_zone_names,
                                   geozone_list=NULL,
                                   min_polygon_size=50,
                                   region_IDs=NULL,
                                   create_polygon=FALSE,add_site_to_polygon=TRUE,
                                   create_spatial_files = FALSE,
                                   apply_fix = FALSE, 
                                   unique_id_mappings = NULL,
                                   carrying_capacity_adjustments = NULL,
                                   cluster_path=NULL,
                                   existing_cluster_file=NULL){
  
  # Set a global seed for reproducibility
  set.seed(123)
  
  if(create_polygon){
    source("01_step4_CreateSpatialFiles/01_Run_CreateSitePolygons.R")
    create_site_polygons(output_directory, cluster_properties, bathymetric_file,bathymetric_path,region_IDs,reef_outline_file,reef_geomorphic_path,
                         reef_geomorphic_file,site_size,reshex,resolution,unit,geozone_list,geo_zone_names,min_polygon_size)
  }
  
  if(add_site_to_polygon){
    #this runs adding the spatial polygon
    source("01_step4_CreateSpatialFiles/02_Run_AddSpatialtoSitePolygons.R")
    process_reef_clusters(connectivity_folder,conn_files, cluster_properties, 
                          bathymetric_file,bathymetric_path, ub_files, 
                          ub_files_path,previous_spatial_file,rootdir_data,
                          output_directory,site_polygons_rdata,reef_geomorphic_path,reef_geomorphic_file,gbr_zones_csv,
                          create_spatial_files,apply_fix, unique_id_mappings,carrying_capacity_adjustments,
                          cluster_path,existing_cluster_file)
    
  }
 
  
  
}