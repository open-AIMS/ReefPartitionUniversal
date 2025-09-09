#Anna Note: When possible it would be best not to filter based on the GBR FEATURES shapefile as this does not always align with the geomorphic and benthic habitat map: we may chop off bits of interest. 
#Adjust this before creating site polygons for any future reef clusters

# In current version you need to uncomment/ comment which reef cluster you want

#' @title Create Site Polygons
#'
#' @description This function creates spatial polygons for specified reef clusters using bathymetric and reef data. The function processes the Great Barrier Reef (GBR) spatial data to generate site polygons that can be used for further analysis or modeling.
#' C~scape Site Polygon Generator. Collection of functions to take each geomorphic zone of a reef, pixelate in into hexagons, then cluster these together into sites
#' 
#'
#' @param output_directory A character string specifying the directory where the output files and site polygons will be saved.
#' @param cluster_properties A list containing properties for each reef cluster, such as the cluster name, latitude, longitude, and other relevant parameters.
#' @param bathymetric_file A character string or vector specifying the name(s) of the bathymetric file(s) to be used in creating the site polygons.
#' @param bathymetric_path A character string specifying the directory path where the bathymetric files are located.
#' @param region_IDs A vector of unique IDs representing the Great Barrier Reef sites to be included in the polygon creation process.
#' @param reef_outline_file A character string specifying the path to the shapefile for the Great Barrier Reef. This file contains the spatial outlines of all reefs on the GBR.
#' @param reef_geomorphic_path A character string specifying the directory path where the benthic habitat raster file is located.
#' @param reef_geomorphic_file A character string specifying the name of the benthic habitat raster file (.tif) to be used for extracting habitat data.
#' @param site_size A numeric value specifying the desired size of the site polygons, typically in square meters.
#' @param reshex A character string specifying the resolution of the hexagons to be used in the spatial analysis.
#' @param resolution An integer specifying the resolution level for the hexagons in the H3 grid system (range from 7/5km2-15/0.89m2)).
#' @param unit A character string specifying the unit of measurement for the hexagon area (e.g., "m^2" for square meters).
#' @param geozone_list A vector list to specify to specify which reef zones are to be included 
#' @param geo_zone_names A list of numeric-value pairs, where each element is a vector of length 2. The first element of each vector is a numeric value (to check divisibility),and the second element is the corresponding category name (string) to assign.
#' @param min_polygon_size numeric value indicating the minimum area threshold (in number of hexagons).
#'                        Filters out any polygons smaller than this threshold.
#'                        Default is 50 hexagons (approximately 15,350m² at resolution 12).
#'
#'
#' @details The function performs the following steps:
#' \itemize{
#'   \item Reads in the GBR shapefile to extract reef information and define boundaries for reef clusters.
#'   \item Processes the bathymetric data to generate site polygons for the specified GBR sites.
#'   \item Optionally uses existing shapefiles or creates new spatial folders based on the cluster properties.
#'   \item Creates hexagonal pixels and clusters them into polygons based on the specified site size and other parameters.
#'   \item Performs post-processing on the generated polygons to remove small sites or further refine the polygon geometry.
#'   \item Saves the generated spatial data in various formats, including RData, GeoPackage, and CSV.
#' }
#'
#' @return Returns an `sf` object containing the site polygons with additional attributes such as site ID, habitat type, and area.
#'
#' @importFrom sf st_read st_transform st_centroid st_write
#' @importFrom dplyr filter mutate select slice
#' @importFrom raster raster
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual theme ggsave
#' @importFrom gridExtra grid.table
#' @importFrom purrr map
#' @importFrom utils write.csv
#'
#' @examples
#' # Example usage of the create_site_polygons function
#' cluster_properties <- list(
#'   list(name = "LizardReefCluster", lat = -14, long = 145, use_past_polygon = FALSE, use_exisitng_folder = FALSE),
#'   list(name = "DaviesReefCluster", lat = -18.5, long = 147, use_past_polygon = FALSE, use_exisitng_folder = FALSE)
#' )
#' create_site_polygons(
#'   output_directory = "path/to/output_directory",
#'   cluster_properties = cluster_properties,
#'   bathymetric_file = c("LizardClusterBathy.tif", "DaviesClusterBathy.tif"),
#'   bathymetric_path = "path/to/bathymetry",
#'   region_IDs = c(14116101102, 18084101104),
#'   reef_outline_file = "path/to/GBR_shapefile.shp",
#'   reef_geomorphic_path = "path/to/reefs",
#'   reef_geomorphic_file = "benthic_habitat.tif",
#'   site_size = 1000,
#'   reshex = "high_res",
#'   resolution = 9,
#'   unit = "m^2"
#' )
#'
#' @authors  
#' M Gonzalez Rivero, Vanessa Haller, Anna Cresswell, Justin Moore , Pascal Omondiagbe
#' 
#' 
create_site_polygons <- function(output_directory, cluster_properties, bathymetric_file,bathymetric_path,region_IDs,reef_outline_file,reef_geomorphic_path,reef_geomorphic_file,site_size,reshex,resolution,unit,geozone_list,geo_zone_names,min_polygon_size=50){
  source("CreateSitePolygons_functions.R")
  # read in the GBR features file. This is a shapefile containing the spatial outlines of all reefs on the GBR. It contains the unique IDs that will be used to specify which reefs are included in a modelled cluster. It is best to examine this shapefile in GIS software to spatially explore and define the boundaries of your reef cluster. Then determine what the IDs are and list them here. 
  
    # Print all the input arguments for debugging
    print(paste("Output Directory: ", output_directory))
    print(paste("Cluster Properties: ", cluster_properties))
    print(paste("Bathymetric File: ", bathymetric_file))
    print(paste("Bathymetric Path: ", bathymetric_path))
    print(paste("Region IDs: ", paste(region_IDs, collapse = ", ")))  # Concatenate GBR IDs
    print(paste("reef outline File: ", reef_outline_file))
    print(paste("Reefs Path: ", reef_geomorphic_path))
    print(paste("Reef File: ", reef_geomorphic_file))
    print(paste("Site Size: ", site_size))
    print(paste("Reshex: ", reshex))
    print(paste("Resolution: ", resolution))
    print(paste("Unit: ", unit))


  

    # Convert to sf if needed
    reef_map <- sf::read_sf(reef_outline_file)
    reef_map$UNIQUE_ID<-c(1:nrow(reef_map))
    # reef_map <- reef_map %>%
    #   unite("name", class,UNIQUE_ID , sep = "_", remove = FALSE)
    
    for (Reef_of_Interest in 1:length(cluster_properties)) {
      print(Reef_of_Interest)
      
    
      #i=1
      print(paste(cluster_properties[[Reef_of_Interest]]$name))
      
      cluster_folder = paste(output_directory, cluster_properties[[Reef_of_Interest]]$existing_cluster_folder_name, sep ="/")
      
      cluster_name =cluster_properties[[Reef_of_Interest]]$name
      
      
      # Load the bathymetry file using terra
      bathy_file <- file.path(paste(bathymetric_path, sep = "/"), bathymetric_file[Reef_of_Interest])


      #not in use- Pascal change 
      Lat <- cluster_properties[[Reef_of_Interest]]$lat
      Long <- cluster_properties[[Reef_of_Interest]]$long
      
      name_column <- names(reef_map)[grepl("name", names(reef_map), ignore.case = TRUE)]
     
      # Ensure a valid name column exists
      if (length(name_column) == 0) {
        stop("Error: No column containing 'name' found in the shapefile.")
      } else if (length(name_column) > 1) {
        print(paste("Multiple name columns found:", paste(name_column, collapse = ", ")))
        name_column <- name_column[1]  # Choose the first match
      }
      
    # Do you want to use an existing shapefile of polygons?
    use_past_polygons <- cluster_properties[[Reef_of_Interest]]$use_past_polygon
    
    # TRUE - a folder already exists, FALSE, create a new one
    
    if (!cluster_properties[[Reef_of_Interest]]$use_exisitng_folder){
      timestamp <- format(Sys.time(), "%Y_%m_%d")
      new_spatial_folder <- paste(output_directory, "/", cluster_properties[[Reef_of_Interest]]$name, "_", timestamp, sep = "")
      
      # Ensure the upper directory exists
      if (!dir.exists(output_directory)) {
        dir.create(output_directory, recursive = TRUE)
      }
      # Create the new spatial folder
      dir.create(new_spatial_folder)
      dir.create(paste(new_spatial_folder, "maps", sep = "/"))
      k100_file <- FALSE
    } else {
      list.dirs(output_directory)
      new_spatial_folder <- paste(output_directory, cluster_properties[[Reef_of_Interest]]$existing_cluster_folder_name, sep = "") # AKC - not sure about this, more complicated than necessary??
    }

    
   
    
    # Assuming reef_map is an sf object (if not, convert it)
    if (!inherits(reef_map , "sf")) {
      stop("reef_map is not an sf object. Please check your data.")
    }
    
    # If region_IDs is NULL, use all available UNIQUE_IDs
    if (is.null(region_IDs)) {
      region_IDs <- reef_map$UNIQUE_ID
      print("No specific region_IDs provided. Using all available IDs in reef_map.")
    }
    
    # Filter the reef_map  to contain only those IDs in your chosen cluster (region_IDs)
    # ROI <- reef_map  %>%
    #   dplyr::filter(UNIQUE_ID %in% region_IDs) %>%
    #   st_transform(4326)  # Reproject to WGS84 (EPSG:4326)
      ROI<-reef_map
      region_IDs<-c(1:nrow(ROI))
    
    # Initialize reef_Names
    reef_Names <- vector("character", length(region_IDs))
   
    if (length(reef_Names) > 0) {
      for (i in 1:length(region_IDs)) {
        reef_Names[i] <- reef_map[[name_column]][reef_map$UNIQUE_ID == region_IDs[i]]
      }
    }
    
    
    
    # Check the ROI
    print(ROI)

    
    ##########################################################
    ### Setup spatial polygons for each reef new algorithm ###
    ##########################################################
    
    hab<-list()
    sites<-list()
    
    #reef_geomorphic_file_path= file.path(reef_geomorphic_path,reef_geomorphic_file)
    if (!use_past_polygons) { # only do this if we want to create new polygons
      
      
      
      for (ID in region_IDs) {
        
        index <- match(ID, region_IDs)
        print(ID)
        print(reef_Names[index])
        
        # CreatePixels2 - this creates all the hexagonal pixels and assigns depth
        hab[[which(region_IDs == ID)]] <- CreatePixels(
          reef_name = reef_Names[index], 
          ROI = ROI[ROI$UNIQUE_ID == ID, ], reshex, site_size, bathy_file,
          full_geo_file_path, geozone_list, geo_zone_names, TRUE, resolution
        )
        
        print("Pixels created")
        sites_hab <- do.call(rbind, hab)
      }
      
      # Standardize X, Y, and Depth values
      sites_hab$X_standard <- scale(sites_hab$X)
      sites_hab$Y_standard <- scale(sites_hab$Y)
      sites_hab$Depth_standard <- scale(sites_hab$Depth)
      
      # CreatePolygonsFromPixelsNew clusters the pixels together
      for (ID in region_IDs) {
        
        sites[[which(region_IDs == ID)]] <- CreatePolygonsFromPixelsNew(
          hab.pts = sites_hab[sites_hab$UNIQUE_ID == ID, ], 
          site_size = site_size, 
          reef_name = reef_Names[which(region_IDs == ID)], 
          saveDirectory = new_spatial_folder
        )
        
        print("Pixels clustered")
        
        # Bind reefs together
        sites_cluster <- do.call(rbind, sites)
        saveRDS(sites_cluster, paste(new_spatial_folder, "Spatial_withpolygonGeometry.Rdata", sep = "/"))
        print(reef_Names[which(region_IDs == ID)])
      }
    }
    
    sites<-sites_cluster
    
    
    
    ########################
    #### Post processing ###
    ########################
    
    # Vanessa CHECK - can you add some description of what this is doing please?
    # combining some small sites??
    RowsToRemove<-c()
    ExtraSites<-c("a","b","c","d","e","f")
    NewSites<-sites[1,] #data.frame("site_id","habitat","area","UNIQUE_ID","Reef","geometry")
    ROI_crs= sf::st_crs(ROI)
    
    for (i in 1:nrow(sites)){
      #print(i)
      if (sites$area[i]<min_polygon_size*307/10^6){ #Removes sites that are smaller than a minimum threshold (50 hexagons * 307m²/10⁶) #that 50 is now parameter, use parameter name instead
        RowsToRemove<-c(RowsToRemove,i)
      } else {
        if (class(sites$geometry[i])[1]=="sfc_MULTIPOLYGON"){ #Processing Multi-polygons:
          #When a site consists of multiple polygons (sfc_MULTIPOLYGON)
          #Separates multi-polygons into individual polygons
          #Assigns new IDs using letters (a,b,c,d,e,f)
          #Creates new rows for each separated polygon
          NewPolygons<-PostProcessing(polygon=sites[i,],min_polygon_size,ROI_crs)
          
          RowsToRemove<-c(RowsToRemove,i)
     
          NewRows<-sites[i,]%>% slice(rep(1:n(), each = nrow(NewPolygons)))
          for (m in 1:nrow(NewPolygons)){
            NewRows$geometry[m]<-NewPolygons$geometry[m]
          }
          
          NewRows$site_id<-paste0(NewRows$site_id,ExtraSites[1:nrow(NewPolygons)])
          
          NewSites<-rbind(NewSites,NewRows)
        }
      }
      
    }
    sites<-sites[-RowsToRemove,]
    NewSites<-NewSites[-1,]
    sites<-rbind(sites,NewSites)
    
    saveRDS(sites, paste(new_spatial_folder, "Spatial_withpolygonGeometry_PostProcessing.Rdata", sep="/"))

  
    ####################################
    #Plot clusters######################
    ####################################

    qual_col_pals = brewer.pal.info
    col_vector = rev(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))
    col_vector<-c(col_vector,col_vector,col_vector,col_vector)


    ggplot() +
      geom_sf(data = subset(sites), aes(fill = site_id, geometry = geometry), colour = "black") +
      annastheme +
      scale_fill_manual(values = col_vector) +
      theme(legend.key.size = unit(0.1, 'cm'), #change legend key size
            legend.key.height = unit(0.3, 'cm'), #change legend key height
            legend.key.width = unit(0.3, 'cm'), #change legend key width
            legend.title = element_text(size=8), #change legend title font size
            legend.text = element_text(size=8)) +#change legend text font size
      theme(legend.position="none")

    ggsave(paste(new_spatial_folder,"/maps/Cluster_map_largerPostProcessing.png",sep=""), width = 30, height = 20, dpi = 600)


    # png(paste(new_spatial_folder,"/maps/SiteNumbers.png",sep=""), height=400, width=400)
    # p<-table(sites$Reef,sites$habitat)
    # grid.table(p)
    # dev.off()


    #SAVING
    
    # save geopackage ----
    st_write(sites,   paste0(new_spatial_folder,"/",cluster_name, "_Polygon_Geometry.gpkg"))
    
    
    #save Rdata file with point info only----
    cluster_point<-sites %>%
       st_centroid()
    # saveRDS(object = cluster_point, file = paste0(new_spatial_folder,"/",cluster_name, "_Point_Geometry.Rdata"))
    
    #save csv dataframe file with lat long columns----
    cluster_df <- cluster_point %>%
      mutate(long = unlist(map(cluster_point$geometry,1)),
             lat = unlist(map(cluster_point$geometry,2)))
    
    cluster_csv = as.data.frame(cluster_df) %>%
      dplyr::select(-geometry)
    

    
    write.csv(cluster_csv, paste0(new_spatial_folder,"/",cluster_name, "_Spatial.csv"), row.names = F)
    # 
    # saveRDS(object = cluster_csv, file = paste0(cluster_name, "_dataframe.Rdata")) #this used by IPMF model
    
    #will go to new_spatial_folder - copy to project data folder
    
    }
  return(sites)
} 
