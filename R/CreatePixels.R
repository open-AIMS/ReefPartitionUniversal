#' @export
CreatePixels <- function(reef_name, ROI, reshex, site_size, bathy_file, full_geo_file_path,geozone_list,geo_zone_names,overwrite = T,resolution=12) {
  
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
    bbox_geo_hab <- sf::st_bbox(geo_hab)  # Get raster bounding box
    bbox_ROI <- sf::st_bbox(ROI)  # Get ROI bounding box
    
    # Adjust ROI xmin and xmax if outside
    if (bbox_ROI["xmin"] < bbox_geo_hab["xmin"]) bbox_ROI["xmin"] <- bbox_geo_hab["xmin"]
    if (bbox_ROI["xmax"] > bbox_geo_hab["xmax"]) bbox_ROI["xmax"] <- bbox_geo_hab["xmax"]
    
    # Adjust ROI ymin and ymax if outside
    if (bbox_ROI["ymin"] < bbox_geo_hab["ymin"]) bbox_ROI["ymin"] <- bbox_geo_hab["ymin"]
    if (bbox_ROI["ymax"] > bbox_geo_hab["ymax"]) bbox_ROI["ymax"] <- bbox_geo_hab["ymax"]
    
    # Create the new adjusted ROI
    ROI <- sf::st_as_sfc(bbox_ROI, crs = st_crs(geo_hab))
    
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
  geo_hab_cropped <- stars::st_as_stars(geo_hab_cropped)
  
  
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
    sf::st_as_sf() %>%
    dplyr::rename(class = names(geo_hab_cropped)[1]) %>%
    dplyr::filter(if (!is.null(geozone_list)) class %in% geozone_list else TRUE)
  
  # Check if transformation is needed
  bathy_tif <- terra::rast(bathy_file)
  bathy_crs <- sf::st_crs(bathy_tif)
  
  if (st_crs(geo_hab_sf) != bathy_crs) {
    geo_hab_sf <- sf::st_transform(geo_hab_sf, bathy_crs)
    print("Transformed geo_hab_sf to match bathymetry CRS")
  }
  
  # Generate H3 hexagons with correctly positioned data
  hexid <- h3::geo_to_h3(geo_hab_sf, res = resolution)
  
  
  ########
  #select region of interest from geomorphic habitat map
  geo.hab <- stars::read_stars(full_geo_file_path)%>%
    sf::st_crop(., ROI)
  names(geo.hab)[1] <- "GBR10.GBRMP.Geomorphic.tif"
  
  hexid = geo.hab %>%
    sf::st_as_sf() %>%
    dplyr::rename(class = GBR10.GBRMP.Geomorphic.tif) %>%
    #filter(class %in% c(22,15,14,13,14,15,21))%>% #filter only for habitats of interest (Check selection criteria with Manu)
    dplyr::filter(class %in% geozone_list) %>% #filter only for geomorphic zones to be included
    h3::geo_to_h3(., res = 12) #create hexagons, res is the resolution of hexagons to return res=12~307m2
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

    geo_hab_cropped <- sf::st_transform(geo_hab_cropped, raster::crs(bathy_tif))

    crop_extent <- sf::st_bbox(geo_hab_cropped)
    
    # Ensure that the ROI and the raster have the same CRS
    if (sf::st_crs(bathy_tif) != sf::st_crs(crop_extent)) {
      print(paste("Transforming crop_extent from", sf::st_crs(crop_extent)$input, "to", sf::st_crs(bathy_tif)$input))
      # Transform crop_extent to match bathy_tif's coordinate system
      crop_extent_wgs84 <- sf::st_transform(sf::st_as_sfc(crop_extent), sf::st_crs(bathy_tif))
      crop_extent <- sf::st_bbox(crop_extent_wgs84)
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
    Pixels_transformed <- sf::st_transform(Pixels, sf::st_crs(geo_hab_cropped))
    
    
    Pixels_clean <- Pixels_transformed %>%
      dplyr::filter(!is.na(sf::st_dimension(.))) %>%  # Remove NA dimensions
      sf::st_make_valid()
    
    cells <- sf::st_as_sf(geo_hab_cropped, as_points = TRUE)
    hab_pts <-Pixels_clean %>% 
      dplyr::mutate(id = hexid,
             area = h3::hex_area(res=resolution, unit = unit)) %>% #is this km2 ok?? #Anna - not sure this is actually working
      sf::st_join(., cells, join = sf::st_nearest_feature) %>%
      dplyr::rename(geomorph = "GBR10.GBRMP.Geomorphic.tif") %>%
      # bind_cols( .,
      #            st_extract(x = geo_hab_cropped, at = ., mfv) %>%
      #              st_drop_geometry() %>%
      #              rename(geomorph = colnames(.)[1])
      # )%>%
      #mutate(geomorph = as.integer(geomorph)) %>%
      sf::st_transform(3112) %>% #project to GDA94 / Geosicence Australia Lambert https://epsg.io/3112
      dplyr::bind_cols(., base::as.data.frame(st_coordinates(.))) %>%
      dplyr::filter(!is.na(geomorph), if (!is.null(geozone_list)) geomorph %in% geozone_list else TRUE) %>%  # Handle NULL geozone_list
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
      dplyr::rename(habitat = geomorph)
    
    
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
    
    
  } else {
    hab_pts<-NULL
  }
  
  
  
  return(hab_pts)
}