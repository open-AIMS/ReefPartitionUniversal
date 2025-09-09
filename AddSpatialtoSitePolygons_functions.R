#####################################
# C~scape Spatial information

# Main functions for adding spatial information to site polygons
# Called upon by: Run_SpatialSetup_post_processing_working.R

# Authors: Anna Cresswell, Vanessa Haller, Justin Moore, 

# Date: 27/5/2024
######################################




#' @title Calculate Benthic Habitat Composition
#'
#' @description This function calculates the composition of benthic habitats for each site based on a benthic habitat raster map. It extracts pixel-based habitat data for each site, calculates habitat metrics, and handles missing data by interpolating from the nearest sites with available information.
#'
#' @param sites_reef An `sf` object containing spatial data for reef sites. This dataset is updated with benthic composition metrics.
#' @param outdir A character string specifying the output directory where results will be saved. This parameter is not used within the function but could be relevant for saving results in future modifications.
#' @param reefs_path A character string specifying the directory path where the benthic habitat raster file is located.
#' @param reef_file A character string specifying the name of the benthic habitat raster file (.tif) to be used for extracting habitat data.
#' 
#' @details The function performs the following steps:
#' \itemize{
#'   \item Reads the benthic habitat raster file.
#'   \item Extracts benthic habitat data for each site using the raster.
#'   \item Calculates the amount of each benthic habitat type (e.g., rubble, sand, rock, coral_algae) based on pixel counts.
#'   \item Identifies and handles missing data by interpolating values from the nearest sites with available data.
#'   \item Updates the spatial dataset with calculated habitat metrics and the most common benthic category.
#'   \item Adds area information for each site and creates an ID column.
#'   \item Uses Euclidean distance to find and replace missing values with data from the nearest site with similar habitat characteristics.
#' }
#' 
#' @return An `sf` object with updated benthic habitat metrics, including the amount of rubble, sand, rock, coral algae, NA pixels, and total pixels. Sites with significant missing data are updated with values from the nearest neighboring sites.
#' 
#' @examples
#' # Example usage of the CalculateBenthic function
#' sites_reef <- st_read("path/to/reef_sites_shapefile.shp")
#' outdir <- "path/to/output_directory"
#' reefs_path <- "path/to/reefs_directory"
#' reef_file <- "benthic_habitat_file.tif"
#' updated_sites <- CalculateBenthic(sites_reef, outdir, reefs_path, reef_file)
#' 
#' @authors  
#' Anna Cresswell, Vanessan Haller, Justin Moore ,  Pascal Omondiagbe
#' 
#' @importFrom terra rast
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_transform st_point_on_surface st_distance st_area
#' @importFrom dplyr %>% mutate select
#' 
CalculateBenthic<-function(sites_reef, outdir,reefs_path,reef_file){
  ##########################################################################################
  ## read in the benthic map and use pixel type to calculate benthic composition of each site
  ##########################################################################################
  #read in the benthic habitat file
  benthic.hab <- terra::rast(file.path(reefs_path,
                                       reef_file))
  benthic.calc <- exactextractr::exact_extract(benthic.hab, sites_reef ,append_cols=T, fun=c('count','frac'), default_value = 0)

  # Check if any rubble pixels were found or replace with all 0's
  if ("frac_12" %in% names(benthic.calc) && is.data.frame(benthic.calc)) {
    rub <- round(benthic.calc$frac_12 * benthic.calc$count) #multiply the proportion of pixels by the total count of pixels
  } else {
    rub <- numeric(nrow(benthic.calc)) #if none then set rubble for all sites to be zero
  }
  
  # Check if any sand pixels were found or replace with all 0's
  if ("frac_11" %in% names(benthic.calc) && is.data.frame(benthic.calc)) {
    sand <- round(benthic.calc$frac_11 * benthic.calc$count)
  } else {
    sand <- numeric(nrow(benthic.calc))
  }
  
  # Check if any rock pixels were found or replace with all 0's
  if ("frac_13" %in% names(benthic.calc) && is.data.frame(benthic.calc)) {
    rock <- round(benthic.calc$frac_13 * benthic.calc$count)
  } else {
    rock <- numeric(nrow(benthic.calc))
  }
  
  # Check if any coral_algal pixels were found or replace with all 0's
  if ("frac_15" %in% names(benthic.calc) && is.data.frame(benthic.calc)) {
    coral_al <- round(benthic.calc$frac_15 * benthic.calc$count)
  } else {
    coral_al <- numeric(nrow(benthic.calc))
  }
  
  # Check if any NA pixels were found or replace with all 0's
  if ("frac_0" %in% names(benthic.calc) && is.data.frame(benthic.calc)) {
    na_pixel <- round(benthic.calc$frac_0 * benthic.calc$count)
  } else {
    na_pixel <- numeric(nrow(benthic.calc))
  }

  total_pixel<-round(benthic.calc$count) #this is total pixels per site

  values <- data.frame(rub, sand, rock, coral_al, na_pixel)
  names(values) <- c( "Rubble","Sand", "Rock","Coral_Algae", "NaN")
  
  benth_mode <- vector("character", length = nrow(benthic.calc)) #create empty vector
  
  #assign which is the most common category
  for( z in 1:nrow(benthic.calc)){
    benth_mode[z] <- names(values)[which.max(colSums(values[z,]))]
  } 


  sites_reef<-sites_reef%>% 
   mutate(benthic_mode = benth_mode,
                      rubble=rub,
           sand=sand,
           rock = rock,
           coral_algae = coral_al,
           na_pixel = na_pixel,
           total_pixel=total_pixel,
           habitat=as.factor(habitat),
           reef_siteid=as.character(site_id),
           UNIQUE_ID=UNIQUE_ID,
           Reef=Reef,
           Surrogate="nil") #field to determine if the mode was NaN and which site replaced the values
  

  #ADD AREA COLUMN
  sites_reef$area<-st_area(sites_reef) #, unit="m2"
  sites_reef$ID<-c(1:nrow(sites_reef)) 
  
  
  
  ## DECIDE WHAT TO DO IF NAS
  #take values from the nearest polygon with data of the same geomorphic zone  
  #The centroids:
  shp_centroid <- st_point_on_surface(x =sites_reef)

  #The euclidian distance matrix:
  mtx_distance <- st_distance(shp_centroid,shp_centroid)
  
  # Set a limit for how many tries you want, here the maximum number of sites in the reef
  max_try <- nrow(sites_reef)
  
  
  # Anna changing slightly here, only take values from an adjacent site polygons if <5% of cells have benthic information
  # rather than if the mode is NaN
  # i.e. preferentiate using any available information from within the site polygon rather than looking to another site polygon
  
  #keep original pixel count values
  sites_reef$rubble_raw = sites_reef$rubble
  sites_reef$sand_raw = sites_reef$sand
  sites_reef$rock_raw = sites_reef$rock
  sites_reef$coral_algae_raw = sites_reef$coral_algae
  sites_reef$na_pixel_raw = sites_reef$na_pixel
  sites_reef$total_pixel_raw = sites_reef$total_pixel
  
  
  # if more than 95% of the pixels are NAs then extract information from the nearest adjacent polygon
  #sites_NA <- sites_reef$ID[sites_reef$benthic_mode == "NaN"] #justin version 

  sites_reef$replace_values = ((sites_reef$na_pixel/ sites_reef$total_pixel)>0.95) #anna version
  sites_NA = which(sites_reef$replace_values)
  if (sum(sites_reef$replace_values) > 0 & nrow(sites_reef)>sum(sites_reef$replace_values)) {
    for (i in 1:sum(sites_reef$replace_values)) {   #for each site that has an NA
      print(paste(i,as.character(sites_reef$reef_siteid[sites_NA[i]]), sep = ", "))
      Sorted <- sort(mtx_distance[sites_NA[i], ])   #sort locations by distance from the NA site
      try <- 2
      #while (sites_reef$benthic_mode[sites_NA[i]] == "NaN" && try <= max_try) {    #while you haven't found a solution yet keep going #Justins version
      while (sites_reef$replace_values[sites_NA[i]] && try <= max_try) {    #while you haven't found a solution yet keep going #Anna's version
      
        Index <- which(mtx_distance[sites_NA[i], ] == Sorted[try]) #identify index of the nearest site
        if (length(which(Index == sites_NA)) == 0) { #check that the nearest neighbour is not another site which has more than 95% NAs
          Matches <- sites_reef$habitat[sites_NA[i]] == as.character(sites_reef$habitat[Index]) #check whether habitat matches. e.g. is it also a slope site?
        } else {
          Matches <- FALSE
        }

        if (Matches == TRUE) {
          print("Found a match")
          # give this polygon with >95% NA the values of the nearest polygon with information for each benthic category
          sites_reef$benthic_mode[sites_NA[i]] <- sites_reef$benthic_mode[Index]
          sites_reef$rubble[sites_NA[i]] <- sites_reef$rubble[Index]
          sites_reef$sand[sites_NA[i]] <- sites_reef$sand[Index]
          sites_reef$rock[sites_NA[i]] <- sites_reef$rock[Index]
          sites_reef$coral_algae[sites_NA[i]] <- sites_reef$coral_algae[Index]
          sites_reef$na_pixel[sites_NA[i]] <- sites_reef$na_pixel[Index]
          sites_reef$total_pixel[sites_NA[i]] <- sites_reef$total_pixel[Index]
          sites_reef$Surrogate[sites_NA[i]] <- as.character(sites_reef$site_id[Index]) #writes the site from which habitat data was taken from
          
          sites_reef$replace_values[sites_NA[i]] = FALSE #change to FALSE because you have now replaced this
          
        } else {
          try <- try + 1
          print(try)
        }
        # Add a condition to break the loop when try exceeds max_try
        if (try > max_try) { 
          break   
          print(paste("No match found"))
        }
      }
    
      }
 
  } 
  sites_reef = sites_reef %>%
    dplyr::select(-replace_values) #
  return(sites_reef)
}



CalculateK_YM<-function(sites_Habitat){
  # a<-sum(sites_Habitat$rubble)/sum(sum(sites_Habitat$rock),sum(sites_Habitat$coral_algae))
  # b<-sum(sites_Habitat$sand)/sum(sum(sites_Habitat$rock),sum(sites_Habitat$coral_algae))  
  # c<-sum(sum(sites_Habitat$sand),sum(sites_Habitat$rubble))/sum(sites_Habitat$rock)  
  # d<-sum(sum(sites_Habitat$sand),sum(sites_Habitat$rubble))/sum(sites_Habitat$coral_algae)   
  #   
  p<-0.4
  q<-1-p
  
  sand<-sites_Habitat$sand/(sites_Habitat$total_pixel-sites_Habitat$na_pixel)
  rubble<-sites_Habitat$rubble/(sites_Habitat$total_pixel-sites_Habitat$na_pixel)
  rock<-sites_Habitat$rock/(sites_Habitat$total_pixel-sites_Habitat$na_pixel)
  coral_algae<-sites_Habitat$coral_algae/(sites_Habitat$total_pixel-sites_Habitat$na_pixel)
  
  noncolonisable = (p+q*a)*sand+ (p+q*b)*rubble + q*c*rock + q*d*coral_algae
  CarryingCapacity<-1-noncolonisable
  return(CarryingCapacity)
}



#' @title Add Depth and Area Information to Spatial Data
#'
#' @description This function integrates bathymetric data into a spatial dataset by adding depth and 3D area information. It ensures that both the bathymetric raster and the spatial sites data share the same coordinate reference system, and calculates additional metrics like slope and area ratios.
#'
#' @param sites_all An `sf` object containing spatial data for the sites. This dataset is integrated with bathymetric information.
#' @param bathy_file A character string specifying the path to the bathymetric raster file (.tif) that contains elevation data for the region of interest.
#' 
#' @details The function performs the following tasks:
#' \itemize{
#'   \item Reads and processes the bathymetric raster file.
#'   \item Transforms the coordinate reference system of the sites data if necessary.
#'   \item Crops the bathymetric raster to match the extent of the sites data.
#'   \item Computes slope from the bathymetric data.
#'   \item Calculates 3D area for each pixel and extracts these values for each site.
#'   \item Extracts depth values for each site and computes summary statistics (mean, standard deviation, median).
#'   \item Handles missing depth data by interpolating from the nearest neighbors.
#'   \item Joins depth and area information to the sites data and returns the updated spatial dataset.
#' }
#' 
#' @return An `sf` object with additional columns for depth and area metrics, including `depth_mean`, `depth_sd`, `depth_med`, `area2d`, `area3d`, and `ratio2d3d`.
#' 
#' @examples
#' # Example usage of the AddDepth function
#' sites_all <- st_read("path/to/sites_shapefile.shp")
#' bathy_file <- "path/to/bathymetric_file.tif"
#' updated_sites <- AddDepth(sites_all, bathy_file)
#' 
#' @authors  
#' Anna Cresswell, Vanessan Haller, Justin Moore ,  Pascal Omondiagbe
#' 
#' @importFrom raster raster crop terrain extract
#' @importFrom sf st_transform st_crs st_coordinates st_geometry
#' @importFrom dplyr %>% mutate summarise group_by left_join drop_na
#' @importFrom RANN get.knnx
#'
AddDepth<-function(sites_all,bathy_file){
  ##ADD BATHYMETRY - ANNA CODE ----
  #requires a .tif of region of interest 
  bathy_tif = raster( bathy_file) 
  
  print(paste(crs(bathy_tif)))
  print(paste(st_crs(sites_all)))
  
  # Ensure both layers share the same CRS
  if (st_crs(sites_all) != crs(bathy_tif)) {
    sites_all <- st_transform(sites_all, crs(bathy_tif))
  }  
  bathy_tif <- crop(bathy_tif, extent(sites_all))
  
  bathy_tif_slope = terrain(bathy_tif, opt='slope', unit='radians', neighbors=8)
  

  
  # # Define the x and y limits (replace with your desired limits)
  # xlim <- c(413000, 421000) # Example limits based on your sf data bounding box
  # ylim <- c(8132000, 8138000)   # Example limits based on your sf data bounding box
  # 
  # png("Slope_degrees.png", width = 1600, height = 1200, res = 300)
  # plot(bathy_tif, main = "Slope Raster with Sites Overlay", xlim = xlim, ylim = ylim, legend.args = list(text = "Slope in degrees from bathy map", side = 4, font = 2, line = 2.5, cex = 0.8), cex.main = 0.5)
  # plot(st_geometry(sites_all), add = TRUE, border = "black", lwd = 0.1)
  # dev.off()
  
  
  

  area_tif3d = (10*10)/((cos(bathy_tif_slope))) # 10x10m (size of bathy grid cell)
  

  
  # png("bathy_with_sites_no_fill.png", width = 1600, height = 1200, res = 300)
  # plot(area_tif3d, main = "3D Area Raster with Sites Overlay", xlim = xlim, ylim = ylim, legend.args = list(text = "3D surface area per 10x10m pixel", side = 4, font = 2, line = 2.5, cex = 0.8), cex.main = 0.5)
  # plot(st_geometry(sites_all), add = TRUE, border = "black", lwd = 0.1)
  # dev.off()
  
  area_extract <- raster::extract(area_tif3d, sites_all, df = TRUE) # extract 3D area values for each tif pixel, should always be above 100, if more slopey site, bigger number
  
  #give poly dataframe a ID to match these
  sites_all$ID = 1:nrow(sites_all)
  area_extract <- drop_na(area_extract)
  area_extract$area2d <- 100
  names(area_extract) <- c("ID", "area3d", "area2d")
  area_extract <- area_extract %>% 
    group_by(ID) %>% 
    summarise(area2d = sum(area2d), 
              area3d = sum(area3d)) %>%
    mutate(ratio2d3d = area3d/area2d)#ratio of 2D to 3D

  depth_extract <- raster::extract(bathy_tif, sites_all, df = TRUE) # extract depth values for all pixels within each site
  depth_extract <- drop_na(depth_extract)
  names(depth_extract) <- c("ID", "depth")
  depth_extract <- depth_extract %>% 
    group_by(ID) %>% 
    summarise(depth_mean = -mean(depth), 
              depth_sd = sd(depth),
              depth_med = -median(depth))


  #add area and depth to spatial file and make +ve
  sites_all = sites_all %>%
    left_join(depth_extract, by = "ID") %>%
    left_join(area_extract, by = "ID") 
    
  
  sites_all <- sites_all %>%
    mutate(index = row_number())     # Use a temporary index to identify rows
  
  # Separate rows with NA in depth_med
  na_rows <- sites_all %>% filter(is.na(depth_med))
  non_na_rows <- sites_all %>% filter(!is.na(depth_med))
  
  # Find nearest non-NA neighbors
  if(nrow(na_rows) > 0) {
    paste("site with no wave information")
    non_na_rows <- st_cast(non_na_rows, "POLYGON")
    na_rows <- st_cast(na_rows, "POLYGON")
    
    nearest_neighbors <- get.knnx(st_coordinates(non_na_rows), st_coordinates(na_rows), k = 1)
    
    # Extract the depth_med values from nearest neighbors
    nearest_ub_med <- non_na_rows$depth_med[nearest_neighbors$nn.index]
    
    # Replace NA depth_med values with nearest non-NA depth_med values
    sites_all <- sites_all %>%
      mutate(depth_med = ifelse(is.na(depth_med), nearest_ub_med, depth_med))
  }
  
  sites_all = sites_all %>%
    dplyr::select(-index)
  
  return(sites_all)
}




#' @title Add Wave Exposure Information to Spatial Data
#'
#' @description This function integrates wave exposure data into a spatial dataset by adding wave exposure metrics. It processes a raster file containing wave data, extracts relevant values, and handles missing data by interpolating from the nearest neighbors.
#'
#' @param sites_all An `sf` object containing spatial data for the sites. This dataset is updated with wave exposure information.
#' @param Ubtif A character string specifying the name of the wave exposure raster file (.tif) located in the specified directory.
#' @param ub_files_path A character string specifying the path to the directory containing the wave exposure raster files.
#' 
#' @details The function performs the following tasks:
#' \itemize{
#'   \item Reads and processes the wave exposure raster file.
#'   \item Transforms the coordinate reference system (CRS) of the sites data to match that of the raster.
#'   \item Extracts wave exposure values from the raster for each site.
#'   \item Filters out negative values, which are considered as no data.
#'   \item Calculates mean, standard deviation, and median wave exposure for each site.
#'   \item Handles missing wave exposure data by interpolating from the nearest non-missing values.
#'   \item Joins wave exposure metrics with the sites data and returns the updated spatial dataset.
#' }
#' 
#' @return An `sf` object with additional columns for wave exposure metrics, including `ub_mean`, `ub_sd`, and `ub_med`.
#' 
#' @examples
#' # Example usage of the AddUb function
#' sites_all <- st_read("path/to/sites_shapefile.shp")
#' Ubtif <- "wave_exposure_file.tif"
#' ub_files_path <- "path/to/ub_files_directory"
#' updated_sites <- AddUb(sites_all, Ubtif, ub_files_path)
#' 
#' Anna Cresswell, Vanessan Haller, Justin Moore ,  Pascal Omondiagbe
#' 
AddUb<-function(sites_all,ubtif,ub_files_path){
  
  
  Ub_tif = raster(file.path(ub_files_path, ubtif)) 
  # this tif contains very negative values instead of NAs I think.
  # filter out these instances?
  sites_all <- st_transform(sites_all, crs(Ub_tif))
  
  ub_extract <- raster::extract(Ub_tif, sites_all, df = TRUE) # extract 3D area values for each tif pixel
  names(ub_extract) <- c("ID", "Ub")
  ub_extract <- subset(ub_extract, Ub >0) #subset out the negative values, they are where there is no wave data 
  ub_extract <- drop_na(ub_extract)
  

  ub_extract <- ub_extract %>% 
    group_by(ID) %>% 
    summarise(ub_mean = mean(Ub), 
              ub_sd = sd(Ub),
              ub_med = median(Ub))
  
  
  #add area and depth to spatial file and make +ve
  sites_all = sites_all %>%
    left_join(ub_extract, by = "ID") 
  
  
  sites_all <- sites_all %>%
    mutate(index = row_number())     # Use a temporary index to identify rows
  
  # Separate rows with NA in ub_med
  na_rows <- sites_all %>% filter(is.na(ub_med))
  non_na_rows <- sites_all %>% filter(!is.na(ub_med))
  
  # Find nearest non-NA neighbors
  if(nrow(na_rows) > 0) {
    paste("site with no wave information")
    nearest_neighbors <- get.knnx(st_coordinates(non_na_rows), st_coordinates(na_rows), k = 1)
    
    # Extract the ub_med values from nearest neighbors
    nearest_ub_med <- non_na_rows$ub_med[nearest_neighbors$nn.index]
    
    # Replace NA ub_med values with nearest non-NA ub_med values
    sites_all <- sites_all %>%
      mutate(ub_med = ifelse(is.na(ub_med), nearest_ub_med, ub_med))
  }
  
  sites_all = sites_all %>%
    dplyr::select(-index)
  
  return(sites_all)
}

