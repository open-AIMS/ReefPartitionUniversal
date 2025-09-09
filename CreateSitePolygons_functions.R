#####################
# C~scape Site Polygon Generator
# Collection of functions to take each geomorphic zone of a reef, pixelate in into hexagons, then cluster these together into sites

# Authors: M Gonzalez Rivero, Vanessa Haller, Anna Cresswell, Justin Moore, Pascal 

# Date: 1/10/2023
#####################

# not call- Not certain what this is doing? 3D area?
get_3d<-function(x){
  tot3d=sum(x)
  return(tot3d)
}


## not call-  Extract Initials from Reef Name to set site ID
makeInitials <- function(charVec) {
  make.unique(vapply(strsplit(toupper(charVec), " "), 
                     function(x) paste(substr(x, 1, 1), collapse = ""), 
                     vector("character", 1L)))
}

#' @title Create Pixels for Geomorphic Zones
#'
#' @description This function processes a selected region of interest (ROI) by creating hexagonal pixels for geomorphic zones. It reads a geomorphic habitat map, generates hexagons, adds bathymetric depth information, and assigns habitat classifications based on geomorphic features.
#'
#' @param reef_name A character string specifying the name of the reef.
#' @param ROI A spatial object defining the region of interest to be processed.
#' @param rootdir_data A character string specifying the root directory for data.
#' @param reshex An integer specifying the resolution of hexagons to be generated.
#' @param site_size A numeric value specifying the size of the site for hexagon creation.
#' @param bathy_file A higher-resolution bathymetric raster with finer spatial detail, where cell values are interpolated using bilinear interpolation, providing a more granular representation of underwater topography.
#' @param full_geo_file_path A character string specifying the path to the geomorphic habitat map.
#' @param overwrite A logical value indicating whether to overwrite existing files. Default is TRUE.
#' @param geo_zone_names A list of numeric-value pairs, where each element is a vector of length 2. The first element of each vector is a numeric value (to check divisibility),and the second element is the corresponding category name (string) to assign.
#'
#' @details The function performs the following tasks:
#' \itemize{
#'   \item Reads the geomorphic habitat map and selects the region of interest.
#'   \item Generates hexagonal pixels for the specified geomorphic zones.
#'   \item Adds bathymetric depth information to the hexagons.
#'   \item Assigns habitat classifications based on geomorphic features.
#'   \item Filters and processes the data to create a final output for further analysis.
#' }
#' 
#' @return Returns a spatial object with hexagon centers and associated geomorphic habitat classifications.
#' 
#' @examples
#' reef_name <- "Heron Reef"
#' ROI <- read_sf("path_to_ROI_file")
#' rootdir_data <- "/path/to/data"
#' reshex <- 12
#' site_size <- 100
#' bathy_file <- "path_to_bathy_file.tif"
#' full_geo_file_path <- "path_to_geomorphic_habitat_map.tif"
#' pixels <- CreatePixels2(reef_name, ROI, rootdir_data, reshex, site_size, bathy_file, full_geo_file_path)
#' 
#' @authors  
#' Vanessan Haller, Anna Cresswell, Justin Moore , Pascal Omondiagbe
#' 
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom raster raster extract disaggregate
#' @importFrom dplyr mutate filter case_when rename bind_cols
#' @importFrom h3 geo_to_h3 h3_to_geo_sf h3_is_valid hex_area
# Pixelate for each geomorphic zone

CreatePixels <- function(reef_name, ROI, reshex, site_size, bathy_file, full_geo_file_path,geozone_list,geo_zone_names,overwrite = T,resolution=12) {
 
  
  # Detect if full_geo_file_path is a GeoJSON, convert if necessary
  if (grepl("\\.geojson$", full_geo_file_path, ignore.case = TRUE)) {
    print("GeoJSON detected. Converting to GeoTIFF...")
    
    # Define the output GeoTIFF file path
    reef_tif_path <- sub("\\.geojson$", ".tif", full_geo_file_path, ignore.case = TRUE)
    
    # Define target UTM projection (adjust based on region)
    target_crs <- st_crs(ROI) # Use ROI CRS as the target
    
    # Convert GeoJSON to GeoTIFF
    full_geo_file_path <- convert_geojson_to_geotiff(full_geo_file_path, reef_tif_path, target_crs)
  }
  
  ## read in the geomorphic habitat map, the base map for clustering hexagons to create polygons
  #select region of interest from geomorphic habitat map
  geo_hab <- stars::read_stars(
    full_geo_file_path)
  
  
  # Ensure that the ROI and the raster have the same CRS
  if (sf::st_crs(geo_hab) != sf::st_crs(ROI)) {
    print("transforming geo_hab raster")
    ROI <- sf::st_transform(ROI, st_crs(geo_hab))
  }
  # 
  # Check if ROI intersects with the raster extent
  intersects <- sf::st_intersects(sf::st_as_sfc(sf::st_bbox(geo_hab)), ROI, sparse = FALSE)
  

  # Check valid overlap and dimensions before attempting crop
  overlap <- sf::st_intersects(ROI, st_as_sfc(st_bbox(geo_hab)), sparse = FALSE)
  
  if (!any(overlap)) {
    print("ROI is outside raster extent")
    
    # Step 2: Adjust ROI to Match Raster Extent
    bbox_geo_hab <- st_bbox(geo_hab)  # Get raster bounding box
    bbox_ROI <- st_bbox(ROI)  # Get ROI bounding box
    
    # Adjust ROI xmin and xmax if outside
    if (bbox_ROI["xmin"] < bbox_geo_hab["xmin"]) bbox_ROI["xmin"] <- bbox_geo_hab["xmin"]
    if (bbox_ROI["xmax"] > bbox_geo_hab["xmax"]) bbox_ROI["xmax"] <- bbox_geo_hab["xmax"]
    
    # Adjust ROI ymin and ymax if outside
    if (bbox_ROI["ymin"] < bbox_geo_hab["ymin"]) bbox_ROI["ymin"] <- bbox_geo_hab["ymin"]
    if (bbox_ROI["ymax"] > bbox_geo_hab["ymax"]) bbox_ROI["ymax"] <- bbox_geo_hab["ymax"]
    
    # Create the new adjusted ROI
    ROI <- st_as_sfc(bbox_ROI, crs = st_crs(geo_hab))
    
    print("✅ ROI has been adjusted to match raster extent.")
  }
    
  
  
  geo_hab_terra <- terra::rast(geo_hab)
  #ROI<-ROI[20:25,]
  ROI_terra <- terra::vect(ROI)
 
  geo_hab_cropped <- terra::crop(geo_hab_terra, ROI_terra)
  
  
  # ROI_adjusted <- st_bbox(c(
  #   xmin = max(st_bbox(ROI)[1], st_bbox(geo_hab)[1]),
  #   ymin = max(st_bbox(ROI)[2], st_bbox(geo_hab)[2]),
  #   xmax = min(st_bbox(ROI)[3], st_bbox(geo_hab)[3]),
  #   ymax = min(st_bbox(ROI)[4], st_bbox(geo_hab)[4])
  # ))
  # 
  # 
  
  if (terra::ncell(geo_hab_cropped) == 0) {
    stop("Cropping resulted in empty raster - check if ROI overlaps with valid raster data")
  }
  
  # Convert back to stars if needed
  geo_hab_cropped <- st_as_stars(geo_hab_cropped)
  
  
  # If there's no intersection, print a message
  if (!any(intersects)) {
    print("ROI does not overlap with the raster extent.")
    hab_pts<-NULL
    return(hab_pts)
  } else {
    print("ROI overlaps with the raster extent.")
  }
  

  names(geo_hab_cropped)[1] <- "GBR10.GBRMP.Geomorphic.tif" #gsub(" ", ".", names(geo_hab_cropped)[1]) #
 
  
  # Make sure geo_hab_cropped is in WGS 84 before generating H3 hexagons
  geo_hab_sf <- geo_hab_cropped %>%
    st_as_sf() %>%
    rename(class = names(geo_hab_cropped)[1]) %>%
    filter(if (!is.null(geozone_list)) class %in% geozone_list else TRUE)
  
  # Check if transformation is needed
  bathy_tif <- terra::rast(bathy_file)
  bathy_crs <- st_crs(bathy_tif)
  
  if (st_crs(geo_hab_sf) != bathy_crs) {
    geo_hab_sf <- st_transform(geo_hab_sf, bathy_crs)
    print("Transformed geo_hab_sf to match bathymetry CRS")
  }
  
  # Generate H3 hexagons with correctly positioned data
  hexid <- h3::geo_to_h3(geo_hab_sf, res = resolution)
  
  
  ########
  #select region of interest from geomorphic habitat map
  geo.hab <- read_stars(full_geo_file_path)%>%
    st_crop(., ROI)
  names(geo.hab)[1] <- "GBR10.GBRMP.Geomorphic.tif"
  
  hexid = geo.hab %>%
    st_as_sf() %>%
    rename(class = GBR10.GBRMP.Geomorphic.tif) %>%
    #filter(class %in% c(22,15,14,13,14,15,21))%>% #filter only for habitats of interest (Check selection criteria with Manu)
    filter(class %in% geozone_list) %>% #filter only for geomorphic zones to be included
    geo_to_h3(., res = 12) #create hexagons, res is the resolution of hexagons to return res=12~307m2
 ##########
  
  
  hexid <- unique(hexid)
  
  
  # hexid = geo_hab_cropped %>%
  #   st_as_sf() %>% # Filter directly on geo_hab: If geo_hab is already an sf object, you can filter directly without converting it again with st_as_sf().
  #   rename(class = names(geo_hab_cropped)[1]) %>%
  #   filter(class %in% geozone_list) %>% #filter only for geomorphic zones to be included
  #   #filter(!!sym(names(geo_hab_cropped)[1]) %in% geozone_list) %>% # Filter directly on the sf object
  #   st_coordinates() %>%       #Extract the coordinates directly before applying the geo_to_h3() function. This approach should reduce unnecessary conversions and streamline the operation, making the process faster.                            # Extract coordinates for h3
  #   h3::geo_to_h3(., res = res) #create hexagons, res is the resolution of hexagons to return res=12~307m2
  # 
  
  
  #only continue here if there are >1 hexagons that fit the selected geomorphic habitats (Anna added this if() statement because was getting errors for very small reefs)
  if (length(hexid)>1) {

    # Load and crop bathymetry data to the same extent as geo_hab_cropped
    bathy_tif <- terra::rast(bathy_file)

    geo_hab_cropped <- st_transform(geo_hab_cropped, crs(bathy_tif))

    crop_extent <- st_bbox(geo_hab_cropped)
    
    # Ensure that the ROI and the raster have the same CRS
    if (sf::st_crs(bathy_tif) != sf::st_crs(crop_extent)) {
      print(paste("Transforming crop_extent from", sf::st_crs(crop_extent)$input, "to", sf::st_crs(bathy_tif)$input))
      # Transform crop_extent to match bathy_tif's coordinate system
      crop_extent_wgs84 <- st_transform(st_as_sfc(crop_extent), st_crs(bathy_tif))
      crop_extent <- st_bbox(crop_extent_wgs84)
    }
    
    bathy_tif_ <- terra::crop(bathy_tif , crop_extent)
    bathy_tif_ <- terra::disagg(bathy_tif_, fact = 5, method = "bilinear")

    
    
    # Disaggregate (upsample) the raster with terra's disagg
    #bathy_tif <- terra::disagg(bathy_tif, fact = 5, method = "bilinear")  # Faster resampling
    
    #bathy_tif <- disaggregate( bathy_tif, 
     #                          fact = 5,
     #                          method = "bilinear") 
    
   
    Pixels<-h3::h3_to_geo_sf(hexid) #Get the centers of the given H3 indexes as sf object.

 
    AddDepth<-raster::extract(bathy_tif_, Pixels, df = TRUE)
  
    colnames(AddDepth)[2]<-"Bathy"
  
    Pixels$Depth<-AddDepth$Bathy
    
    # Transform Pixels to match geo_hab_cropped's CRS
    Pixels_transformed <- st_transform(Pixels, st_crs(geo_hab_cropped))
    
    
    Pixels_clean <- Pixels_transformed %>%
      filter(!is.na(st_dimension(.))) %>%  # Remove NA dimensions
      st_make_valid()
    
    cells <- st_as_sf(geo_hab_cropped, as_points = TRUE)
    hab_pts <-Pixels_clean %>% 
      mutate(id = hexid,
             area = h3::hex_area(res=resolution, unit = unit)) %>% #is this km2 ok?? #Anna - not sure this is actually working
      st_join(., cells, join = st_nearest_feature) %>%
      rename(geomorph = "GBR10.GBRMP.Geomorphic.tif") %>%
      # bind_cols( .,
      #            st_extract(x = geo_hab_cropped, at = ., mfv) %>%
      #              st_drop_geometry() %>%
      #              rename(geomorph = colnames(.)[1])
      # )%>%
      #mutate(geomorph = as.integer(geomorph)) %>%
      st_transform(3112) %>% #project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
      bind_cols(., base::as.data.frame(st_coordinates(.))) %>%
      filter(!is.na(geomorph), if (!is.null(geozone_list)) geomorph %in% geozone_list else TRUE) %>%  # Handle NULL geozone_list
      # mutate(
      #   geomorph = if (!is.null(geo_zone_names)) {
      #     case_when(
      #       !!!map(geo_zone_names, ~expr(geomorph %% !!.x[[1]] == 0 ~ .x[[2]])),
      #       TRUE ~ as.character(geomorph)
      #     )
      #   } else {
      #     as.character(geomorph)  # If geo_zone_names is NULL, retain the original values
      #   }
      # ) %>%
      rename(habitat = geomorph)
    
    
    #old
    # hab_pts <-Pixels_clean %>% 
    #   mutate(id = hexid,
    #          area = h3::hex_area(res=resolution, unit = unit)) %>% #is this km2 ok?? #Anna - not sure this is actually working
    #   bind_cols( .,
    #     st_extract(x = geo_hab_cropped, at = ., mfv) %>%
    #       st_drop_geometry() %>%
    #       rename(geomorph = colnames(.)[1])
    #   )%>%
    #   #mutate(geomorph = as.integer(geomorph)) %>%
    #   st_transform(3112) %>% #project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
    #   bind_cols(., base::as.data.frame(st_coordinates(.))) %>%
    #   filter(!is.na(geomorph), if (!is.null(geozone_list)) geomorph %in% geozone_list else TRUE) %>%  # Handle NULL geozone_list
    #   # mutate(
    #   #   geomorph = if (!is.null(geo_zone_names)) {
    #   #     case_when(
    #   #       !!!map(geo_zone_names, ~expr(geomorph %% !!.x[[1]] == 0 ~ .x[[2]])),
    #   #       TRUE ~ as.character(geomorph)
    #   #     )
    #   #   } else {
    #   #     as.character(geomorph)  # If geo_zone_names is NULL, retain the original values
    #   #   }
    #   # ) %>%
    #   rename(habitat = geomorph)
    print("processing  raster file")
    reef<-strsplit(reef_name," ")[[1]][1]
    if (reef=="U/N"){
      reef<-"UN"
    }
    
    # Check if hab_pts has rows
    if (nrow(hab_pts) > 0) {
      
      # Assign values
      hab_pts$Reef <- reef
      hab_pts$UNIQUE_ID <- ROI$UNIQUE_ID
    } else {
      # If hab_pts already has rows, skip the assignment
      message("hab_pts has no rowa. Skipping the assignment.")
    }
    
    
  } else {
    hab_pts<-NULL
  }
  
  
  
  return(hab_pts)
}


CreatePixels2 <- function(reef_name,ROI, runfolder,rootdir_data,seed,reshex,site.size,bathy_file) {
  #  for (reef in reefs) {
  
  #if (combine_precluster)    Reef = reefs
  set.seed(seed)
  overwrite = T
  
  
  
  # if (combine_precluster) {
  #   saving_name = reefgroup
  #   site.dir <-
  #     paste0(Site_Geometry_Repository,
  #            "/Spatial_",
  #            saving_name,
  #            ".Rdata")
  # } else {
  #   saving_name = sprintf(str_replace(
  #     string = Reef,
  #     pattern = " ",
  #     replacement = "_"
  #   ))
  #   site.dir <-
  #     paste0(Site_Geometry_Repository,
  #            "/Spatial_",
  #            saving_name,
  #            ".Rdata")
  # }
  
  
  # #############################
  # ## 0.  Create Directories ###
  # #############################
  # folder.list = list("data", "model_outputs", "plots")
  # 
  # if (is.null(runfolder)) {
  #   runfolder = paste(str_replace(
  #     string = Reef,
  #     pattern = " ",
  #     replacement = "_"
  #   ),
  #   Sys.Date(),
  #   sep = "_")
  #   root.path = file.path(rootdir_data, "model_runs", runfolder)
  #   lapply(
  #     folder.list,
  #     FUN = function(x) {
  #       dir.create(file.path(root.path, x),
  #                  showWarnings = F,
  #                  recursive = T)
  #     }
  #   )
  #   #create new folder if it doesn't exist for this reef yet
  # } else {
  #   root.path = file.path(rootdir_data, "model_runs", runfolder)
  #   lapply(
  #     folder.list,
  #     FUN = function(x) {
  #       dir.create(file.path(root.path, x),
  #                  showWarnings = F,
  #                  recursive = T)
  #     }
  #   )
  # }
  
  
  
  #############################
  ##### 1.  Generate sites ####
  #############################
  
  # # Option 1 - filter by reef name ----
  # if (reef_lookup == "reefname") {
  #   ROI = GBR_map  %>%
  #     filter(FEAT_NAME %in% "Reef", grepl(Reef, GBR_NAME)) %>%
  #     st_transform(4326)
  #   reef_name = ROI$GBR_NAME
  #   #st_write(ROI, paste0(Reef, "GBRMPAoutline.gpkg"))
  # }
  # 
  # # Option 2 - filter by uniqueID ----
  # if (reef_lookup == "uniqueid") {
  #   ROI = GBR_map  %>%
  #     filter(UNIQUE_ID %in% Reef) %>%
  #     st_transform(4326) #this transforms from GDA to WGS84
  #   reef_name = ROI$UNIQUE_ID
  # }
  # 
  # #check for more than one match and report if so
  # if (combine_precluster) {
  #   print(paste("More than one reef name match, NO:", nrow(ROI)))
  #   reef_name = reefgroup
  # } else {
  #   if (nrow(ROI) > 1) {
  #     print("More than one reef name match")
  #     ROI = ROI[1 , ]
  #     reef_name = ROI$GBR_NAME
  #   }
  # }
  # 
  # print(reef_name)
  
  
  ##########################################################################################
  ## read in the geomorphic habitat map, foundation for clustering hexagons to create polygons
  ##########################################################################################
  
  #select region of interest from geomorphic habitat map
  geo.hab <- read_stars(
    file.path(
      rootdir_data,
      '1_spatial/GIS/Reefs/GBR_habitat_maps/Geomorphic_habitats/GBR10 GBRMP Geomorphic.tif'
    )
  ) %>%
    st_crop(., ROI)
  names(geo.hab)[1] <- "GBR10.GBRMP.Geomorphic.tif"
  
  hexid = geo.hab %>%
    st_as_sf() %>%
    rename(class = GBR10.GBRMP.Geomorphic.tif) %>%
    #filter(class %in% c(22,15,14,13,14,15,21))%>% #filter only for habitats of interest (Check selection criteria with Manu)
    filter(class %in% geozone_list) %>% #filter only for geomorphic zones to be included
    geo_to_h3(., res = 12) #create hexagons, res is the resolution of hexagons to return res=12~307m2
  
  hexid<-unique(hexid)
  
  
  #only continue here if there are >1 hexagons that fit the selected geomorphic habitats (Anna added this if() statement because was getting errors for very small reefs)
  if (length(hexid)) {
    bathy_tif = raster(paste(rootdir_data, '1_spatial/GIS/Bathymetry/GBR', bathy_file, sep = "/")) 
    
    bathy_tif <- disaggregate( bathy_tif, 
                               fact = 5,
                               method = "bilinear")
    Pixels<-h3_to_geo_sf(hexid) 
    AddDepth<-raster::extract(bathy_tif, Pixels, df = TRUE)
    colnames(AddDepth)[2]<-"Bathy"
    Pixels$Depth<-AddDepth$Bathy
    
    cells <- st_as_sf(geo_hab_cropped, as_points = TRUE)
    
    hab.pts <-Pixels %>% #Get the centers of the given H3 indexes as sf object.
      mutate(id = hexid,
             area = hex_area(12, unit = "km2")) %>% #is this km2 ok?? #Anna - not sure this is actually working
      bind_cols(
        .,
        st_extract(x = geo.hab, at = ., mfv) %>%
          # st_extract(x=benthic.hab, at=., FUN=modal)%>% ##Another option -check Manu
          st_drop_geometry() %>%
          rename(geomorph = colnames(.)[1])
      ) %>%
      mutate(geomorph = as.integer(geomorph)) %>%
      st_transform(3112) %>% #project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
      bind_cols(., base::as.data.frame(st_coordinates(.))) %>%
      #st_drop_geometry() %>%
      filter(!is.na(geomorph), geomorph %in% geozone_list) %>%
      mutate(
        geomorph = case_when(
          #rename
          geomorph %% 22 == 0 ~ "Slope",
          geomorph %% 15 == 0 ~ "Crest",
          geomorph %% 14 == 0 ~ "Outer Flat",
          geomorph %% 13 == 0 ~ "Inner Flat",
          geomorph %% 24 == 0 ~ "Back Reef",
          geomorph %% 25 == 0 ~ "Patch",
          geomorph %% 21 == 0 ~ "Sheltered Slope",
          #anna add these for exploration
          geomorph %% 2 == 0 ~ "Deep",
          geomorph %% 11 == 0 ~ "Shallow Lagoon",
          geomorph %% 12 == 0 ~ "Deep Lagoon",
          geomorph %% 23 == 0 ~ "Plateau",
          TRUE ~ as.character(geomorph)
        )
      ) %>%
      rename(habitat = geomorph)
    
    Reef<-strsplit(reef_name," ")[[1]][1]
    if (Reef=="U/N"){
      Reef<-"UN"
    }
    hab.pts$Reef<-paste(Reef,ROI$GBR_ID,sep="_")
    hab.pts$UNIQUE_ID<-ROI$UNIQUE_ID
  } else {
    hab.pts<-NULL
  }
  
  
  
  return(hab.pts)
}







# Vanessa's updated clustering script which incorporates depth and distance between pixels:
#' @title Create Polygons from Hexagonal Pixels
#'
#' @description This function creates polygons from hexagonal pixels, incorporating depth and distance between pixels. It scales the spatial coordinates and depth, clusters the hexagons, and groups them into polygons or multipolygons.
#'
#' @param hab.pts A spatial data frame containing the hexagonal pixels with habitat, coordinates, depth, and other attributes.
#' @param site_size A numeric value specifying the size of the site for clustering.
#' @param reef_name A character string specifying the name of the reef.
#' @param saveDirectory A character string specifying the directory where output files will be saved.
#'
#' @details The function performs the following tasks:
#' \itemize{
#'   \item Removes duplicate hexagonal points.
#'   \item Scales the X and Y coordinates and depth values for clustering.
#'   \item Splits the hexagons by habitat type and clusters them based on proximity and depth.
#'   \item Groups the clustered hexagons into polygons or multipolygons.
#' }
#'
#' @return Returns a spatial object with clustered polygons or multipolygons.
#' 
#' @examples
#' hab.pts <- create_hex_points(reef_name, ROI, rootdir_data, reshex, site_size, bathy_file, full_geo_file_path)
#' saveDirectory <- "output_directory"
#' polygons <- CreatePolygonsFromPixelsNew(hab.pts, site_size, reef_name, saveDirectory)
#' 
#' @authors  
#' Vanessa Haller, Anna Cresswell, Justin Moore , Pascal Omondiagbe
#' @importFrom dplyr distinct scale split bind_rows
#' @importFrom sp proj4string
#' @importFrom rgeos gUnion
CreatePolygonsFromPixelsNew<-function(hab.pts, site_size, reef_name, saveDirectory){
  
  if (nrow(hab.pts)>0) {
    
    hab.pts<-dplyr::distinct(hab.pts) #anna changes here from unique() which was giving an error
    UNIQUE_ID<-unique(hab.pts$UNIQUE_ID)
    Reef<-unique(hab.pts$Reef)
    
    #scale variables
    hab.pts$X_standard<-scale(hab.pts$X)
    hab.pts$Y_standard<-scale(hab.pts$Y)
    hab.pts$Depth_standard<-scale(hab.pts$Depth)
    
    
    #this turns to a list
    
    hab.pts <- hab.pts %>%
      split(., hab.pts$habitat, drop=TRUE)
    
    ##Make Clusters
    hab.pts = lapply(hab.pts,
                     FUN = site_clust3, saveDirectory=saveDirectory)
    hab.pts <- do.call(rbind, hab.pts)
    
    ##Make Sites
    hab.pts <- hab.pts %>% split(., hab.pts$site_id)
    
    #use function 'group_hex' to group hexagons into polygon or multipolygon
    sites <- lapply(hab.pts, group_hex) 
    
    
    sites = do.call(rbind, sites)
    sites$Reef<-Reef
    sites$UNIQUE_ID<-UNIQUE_ID
    
  } else {
    sites<-NULL
  }
  
  
  
  return(sites)
}




# Spatial Clustering of mapped habitats 
site_clust3<-function(x, saveDirectory){
  library(sfnetworks)
  library(igraph)
  library(tidygraph)
  library(spdep)
  library(raster)
  
  if (any(is.na(x$Depth_standard)==TRUE)){
    x$Depth_standard[is.na(x$Depth_standard)==TRUE]<-mean(x$Depth_standard,na.rm=TRUE)
  }
  if (any(is.na(x$Depth)==TRUE)){
    x$Depth[is.na(x$Depth)==TRUE]<-mean(x$Depth,na.rm=TRUE)
  }
  x$ID<-1:nrow(x)
  
  #H3 hexagon average size
  hex_size<-data.frame(Res=c(7:15),
                       Size=c(5161293,737327,105332,15047,2149,307.09,43.87,6.267,0.895))
  
  MinCounts<-round(site_size/hex_size$Size[hex_size$Res==resolution])
  #MinCounts<-200  #How many hexagons as a minimum resolution (hexagon=307m2)
  Extrapolation<-FALSE
  
  if (nrow(x)>10000){
    Extrapolation<-TRUE
    SamplePoints<-sample(c(1:nrow(x)),10000)
    x_old<-x
    x<-x[SamplePoints,]
    
    MinCounts<-MinCounts*(10000/nrow(x_old))
  } 
  if (nrow(x)<1.5*MinCounts){
    x$site_id<-as.factor(paste(x$Reef[1],x$habitat[1],1,sep="_"))
  } else {
    
    coords <- st_centroid(st_geometry(x))
    
    
    #################################
    #### Step 1: Create a graph #####
    #################################
    
    
    # Using triangulation
    tri<-tri2nb(coords)
    Costs_tri<-nbcosts(tri,data = data.frame(Depth=x$Depth),method="manhattan")#,X_standard=x$X_standard,Y_standard=x$Y_standard))
    Costs_tri<-unlist(Costs_tri)
    
    library(expp)
    Edges_tri<-neighborsDataFrame(tri)
    detach("package:expp", unload = TRUE) #AKC - I am needing to do this before expp and raster have conflicting functions
    
    Edges_tri2<-data.frame(from=as.numeric(Edges_tri$id),to=as.numeric(Edges_tri$id_neigh),weights=Costs_tri)
    Network_withEdgesTri<-sfnetwork(x,Edges_tri2[,c(1,2)],directed=FALSE)
    E(Network_withEdgesTri)$weight<-Costs_tri
    
    png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_network_larger.png",sep=""), width = 2000, height = 2500)
    plot(Network_withEdgesTri)
    dev.off()
    
    
    Network_withEdgesTri<-Network_withEdgesTri %>%
      activate("edges") %>%
      mutate(length = edge_length())
    Length_m<-as.numeric(E(Network_withEdgesTri)$length)
    Length_scaled<-scale(as.numeric(E(Network_withEdgesTri)$length))
    Weight_scaled<-scale(as.numeric(E(Network_withEdgesTri)$weight))
    Euclidean_weight<-NA
    for (i in 1:length(Length_m)){
      if (Length_m[i]>30){
        Euclidean_weight[i]<-sqrt(20*Length_scaled[i,1]^2+Weight_scaled[i,1]^2)
      } else {
        Euclidean_weight[i]<-sqrt(Length_scaled[i,1]^2+Weight_scaled[i,1]^2)
      }
    }
    
    Eucliden_weight_old<-sqrt(Length_scaled^2+Weight_scaled^2)
    #Eucliden_weight_old<-sqrt(Length_scaled^2+0.001*Weight_scaled^2) #enable if you want to ignore depth
    E(Network_withEdgesTri)$weight<-Eucliden_weight_old
    
    # Edges_tri2<-data.frame(from=as.numeric(Edges_tri_new$id),to=as.numeric(Edges_tri_new$id_neigh),weights=Costs_tri[as.numeric(E(Network_withEdgesTri)$length)<1000])
    # Network_withEdgesTri<-sfnetwork(x,Edges_tri2[,c(1,2)],directed=FALSE)
    # E(Network_withEdgesTri)$weight<-Edges_tri2[,3]
    # 
    # #png(file = paste("maps/",unique(x$Reef),"_",unique(x$habitat),"_network.png",sep=""), width = 2000, height = 2500)
    # plot(Network_withEdgesTri)
    # #dev.off()
    
    #################################################
    #### Step 3: Create a spanning tree/ forest #####
    #################################################
    
    #Triangulation
    mst_tri<-mst(Network_withEdgesTri,weights=E(Network_withEdgesTri)$weight)#+Length_scaled)
    mst_tri2<-as_sfnetwork(mst_tri)
    
    png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_SpanningTree_larger.png",sep=""), width = 2000, height = 2500)
    plot(mst_tri2)
    dev.off()
    
    #############################
    #### Step 4: Clustering #####
    #############################
    
    #triangulation
    clus10_tri <- skater(edges = get.edgelist(mst_tri), data = data.frame(Depth_standard=x$Depth_standard),crit=MinCounts,vec.crit= rep(1,nrow(x))) # this seems quite intensive in terms of time
    
    png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_clusters_larger.png",sep=""), width = 2000, height = 2500)
    plot((x %>% mutate(clus = clus10_tri$groups))['clus'], main = paste("Cluster number equals ",length(table(clus10_tri$groups)),sep=""))
    dev.off()
    
    sites <- as.factor(paste(x$Reef[1],x$habitat[1],clus10_tri$groups,sep="_"))
    
    if (Extrapolation==TRUE){
      sites <- class::knn(data.frame(x)[,c("X_standard","Y_standard")], 
                          data.frame(x_old)[,c("X_standard","Y_standard")], sites)
      x<-x_old
    }
    
    x$site_id=sites
  }
  
  return(x)
}



#' Convert GeoJSON to GeoTIFF and Reproject
#'
#' Reads a GeoJSON file, checks its coordinate reference system (CRS),
#' reprojects it to the specified target UTM CRS if necessary, and converts it to a rasterised GeoTIFF file.
#'
#' @param geojson_path Character. File path to the input GeoJSON file.
#' @param output_tif_path Character. File path to save the output GeoTIFF file.
#' @param target_crs sf CRS object. The target coordinate reference system (CRS)
#'                   to which the GeoJSON data should be projected.
#'
#' @return Character. The file path of the created GeoTIFF file.
#' @examples
#' target_crs <- st_crs(32655) # Example: UTM zone 55N
#' output_tif <- convert_geojson_to_geotiff("input.geojson", "output.tif", target_crs)
#'
convert_geojson_to_geotiff <- function(geojson_path, output_tif_path, target_crs) {
  
  # Check if output file exists and remove it
  if (file.exists(output_tif_path)) {
    file.remove(output_tif_path)
    print(paste("Removed existing file:", output_tif_path))
  }
  # Read GeoJSON
  geo_data <- st_read(geojson_path)
  
  # Ensure it's projected to the correct CRS
  if (st_crs(geo_data) != target_crs) {
    print("Reprojecting GeoJSON to target UTM CRS...")
    geo_data <- st_transform(geo_data, target_crs)
  }
  
  # Convert to raster (GeoTIFF)
  rast_data <- stars::st_rasterize(geo_data) # Adjust resolution if needed
  
  write_stars(rast_data, output_tif_path, driver = "GTiff", options = "COMPRESS=LZW")
  print(paste("Converted GeoJSON to GeoTIFF:", output_tif_path))
  

  return(output_tif_path)
}




PostProcessing<-function(polygon, min_polygon_size=50,ROI_crs=4326){
  
  PolygonSeperate<-data.frame(index=1:length(polygon$geometry[[1]]))
  NewPolygons<-data.frame(index=1)
  NumberPolygons<-1
  PolygonSeperate$area<-NA
  PolygonSeperate$geometry<-NA
  NewPolygons$geometry<-NA
  PolygonSeperate$Number<-NA
  NewPolygons$area<-NA

  for (lists in 1:length(polygon$geometry[[1]])){
    PolygonSeperate$geometry[lists]<-st_sfc(st_polygon(polygon$geometry[[1]][[lists]]))%>% 
      sf::st_set_crs(ROI_crs)
    PolygonSeperate$area[lists]<-nrow(polygon$geometry[[1]][[lists]][[1]])
  }
  
  while(nrow(PolygonSeperate)>0){
    LargestIndex<-which(PolygonSeperate$area==max(PolygonSeperate$area))[1]
    Dist<-NA
    for (parts in 1:nrow(PolygonSeperate)){
      Dist[parts]<-100000*st_distance(PolygonSeperate$geometry[[LargestIndex]],PolygonSeperate$geometry[[parts]])
    }
    
    if (any(Dist[-LargestIndex]<100)){
      #combine polygons into multipolygon
      Indices<-which(Dist<100)
      Multi<-st_union(st_sfc(PolygonSeperate$geometry[Indices]))
      NewPolygons[NumberPolygons,]<-NumberPolygons
      NewPolygons$geometry[NumberPolygons]<-Multi
      NewPolygons$area[NumberPolygons]<-sum(PolygonSeperate$area[Indices])
      PolygonSeperate<-PolygonSeperate[-Indices,]
      NumberPolygons<-NumberPolygons+1
    } else {
      NewPolygons[NumberPolygons,]<-NumberPolygons
      NewPolygons$geometry[NumberPolygons]<-PolygonSeperate$geometry[LargestIndex]
      NewPolygons$area[NumberPolygons]<-PolygonSeperate$area[LargestIndex]
      PolygonSeperate<-PolygonSeperate[-LargestIndex,]
      NumberPolygons<-NumberPolygons+1
    }
  }
  
  NewPolygons<-NewPolygons[which(NewPolygons$area>min_polygon_size),] #minmum polygon size to filter with
  
  return(NewPolygons)
}



#adjusted mst function from spdep

`mstree_modified` <-
  function(nbw, ini=NULL) {
    n <- length(nbw[[2]])
    nodes <- cbind(FALSE, 0, rep(Inf,n))
    if (is.null(ini))
      ini <- sample(1:n, 1)
    nodes[ini, 1] <- TRUE
    nodes[nbw$neighbours[[ini]], 2] <- ini
    nodes[nbw$neighbours[[ini]], 3] <- nbw$weights[[ini]]
    
    mst <- matrix(0, n-1, 3)
    for (i in 1:(n-1)){
      id.min <- which.min(nodes[,3])
      if (!is.finite(nodes[id.min,3])){
        mst<-FALSE
        break
      }
      #   stop("Graph is not connected!")
      nodes[id.min, 1] <- TRUE
      mst[i, ] <- c(nodes[id.min, 2], id.min, nodes[id.min, 3])
      id.out <- !nodes[nbw$neighbours[[id.min]], 1]
      node.can <- nbw$neighbours[[id.min]][id.out]
      node.cost <- nbw$weights[[id.min]][id.out]
      id.best <- node.cost<nodes[node.can,3]
      nodes[node.can[id.best], 2] <- id.min
      nodes[node.can[id.best], 3] <- node.cost[id.best]
      nodes[id.min, 3] <- Inf
    }
    attr(mst, "class") <- c("mst", "matrix")
    return(mst)
  }

`mstree_modified2` <-
  function(nbw, ini=NULL) {
    n <- length(nbw[[2]])
    nodes <- cbind(FALSE, 0, rep(Inf,n))
    if (is.null(ini))
      ini <- sample(1:n, 1)
    nodes[ini, 1] <- TRUE
    nodes[nbw$neighbours[[ini]], 2] <- ini
    nodes[nbw$neighbours[[ini]], 3] <- nbw$weights[[ini]]
    
    mst <- matrix(0, n-1, 3)
    for (i in 1:(n-1)){
      id.min <- which.min(nodes[,3])
      # if (!is.finite(nodes[id.min,3])){
      #   mst<-FALSE
      #   break
      # }
      #   stop("Graph is not connected!")
      nodes[id.min, 1] <- TRUE
      mst[i, ] <- c(nodes[id.min, 2], id.min, nodes[id.min, 3])
      id.out <- !nodes[nbw$neighbours[[id.min]], 1]
      node.can <- nbw$neighbours[[id.min]][id.out]
      node.cost <- nbw$weights[[id.min]][id.out]
      id.best <- node.cost<nodes[node.can,3]
      nodes[node.can[id.best], 2] <- id.min
      nodes[node.can[id.best], 3] <- node.cost[id.best]
      nodes[id.min, 3] <- Inf
    }
    attr(mst, "class") <- c("mst", "matrix")
    return(mst)
  }



# Union of H3 hexagons to generate site boundary
group_hex<-function(x){
  require(h3)
  require(tidyverse)
  require(sf)
  poly<- h3::h3_set_to_multi_polygon(x$id)%>%
    st_buffer(dist=0 ) %>%
    st_as_sf()%>%
    rename(geometry=x)
  # poly$benthic=unique(x$benthic)
  # poly$geomorph=unique(x$geomorph)
  poly$site_id=unique(x$site_id)
  poly$habitat=unique(x$habitat)
  poly$area=sum(x$area)
  return(poly)
}

# Get mode of habitats
getmode <- function(v) {
  v=v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# Generate Leaflet HTML of habitats and baselayers from GBRMPA Map Server
plot_sites<-function(sites,save.dir, save.plot=T){
  require(sf)
  require(tidyverse)
  require(leaflet)
  require(htmltools)
  require(htmlwidgets)
  require(leaflet.esri)
  
  
  site_markers<-sites%>%
    st_centroid()
  
  #Palettes
  pal.hab <- colorFactor(
    palette = "RdYlBu",levels = levels(sites$habitat))
  
  pal.K <- colorNumeric(
    palette = "RdBu",domain = c(0,100),na.color = "transparent")
  
  
  #Make Plot
  p<-leaflet()%>%addTiles()%>%
    addScaleBar(position = "topleft")%>%
    addPolygons(data=sites, color=pal.hab(sites$habitat), group="Habitat")%>%
    addCircleMarkers(data=site_markers, label = ~site_id, fillColor = pal.K(site_markers$k), fillOpacity = 0.8,
                     stroke = F, group = "Sites")%>%
    addEsriTiledMapLayer(
      url = "https://tiles.arcgis.com/tiles/ll1QQ2mI4WMXIXdm/arcgis/rest/services/SDB_Bathymetry/MapServer",group = "Bathymetry")%>%
    addEsriTiledMapLayer(url="https://tiles.arcgis.com/tiles/ll1QQ2mI4WMXIXdm/arcgis/rest/services/SSR_Sentinel_2018/MapServer", group="Satelite")%>%
    addLegend(pal=pal.hab, values= sites$habitat, title ="Habitat")%>%
    addLegend(pal=pal.K, values=site_markers$k, title ="Carrying Capacity (%)") %>%
    addLayersControl(
      baseGroups = c("Satellite", "Bathymetry"),
      overlayGroups = c("Sites", "Habitat"),
      options = layersControlOptions(collapsed = TRUE))
  
  
  saveWidget(p, file=save.dir)
 
}


annastheme = theme_bw()+
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(1.2,"cm"),
        axis.title=element_text(size=14),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.text = element_text(size = 14, colour = "black"),
        strip.background = element_rect("white"))