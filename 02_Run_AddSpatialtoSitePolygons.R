#remove space
#have had some occasional errors here - maybe something to do with raster or sf packages. 
#also geopackage creation is no longer working




#####################################
### Setup directories ###############
#####################################



#' @title Process Reef Clusters
#'
#' @description This function processes reef clusters by integrating spatial data, bathymetric information, and wave exposure data. It rearranges spatial files to match the order of sites in the connectivity files and projects data to the correct coordinate system.
#'
#' @param conn_files A character vector containing the paths to connectivity files. These files are used to rearrange the spatial file to match the order of sites.
#' @param cluster_properties A list containing the names of the clusters being processed and their respective properties.
#' @param bathymetric_file A character vector containing the paths to bathymetric files that need to be projected to the correct coordinate system.
#' @param ub_files A character vector containing the paths to Ub files required for wave exposure calculations.
#' @param previous_spatial_file A character vector containing the path to the previous spatial file.
#' @param rootdir_data A character string specifying the root directory for data.
#' @param output_directory A character string specifying the directory where output files will be saved.
#' @param site_polygons_rdata A character string specifying the path to the site polygons Rdata file.
#' @param bathymetric_path A character string specifying the path to the bathymetric files directory.
#' @param ub_files_path A character string specifying the path to the Ub files directory.
#' @param connectivity_folder A character string specifying the path to the connectivity folder
#' @param reef_geomorphic_file A character string specifying the reefs file 
#' @param reef_geomorphic_path A character string specifying the path to the reefs file path
#' @param  gbr_zones_csv A full path to the protentection zone csv
#' @param create_spatial_files A logical value indicating whether to create new spatial files (e.g., shapefiles, RData, CSV). Defaults to `FALSE`.
#' @param apply_fix A logical value indicating whether to apply fixes to an existing spatial file (e.g., adjusting UNIQUE_ID or carrying capacity). Defaults to `FALSE`.
#' @param unique_id_mappings A named vector specifying the mappings of old UNIQUE_IDs to new ones. Use this to apply changes to UNIQUE_IDs in the spatial data. Defaults to `NULL`.
#' @param carrying_capacity_adjustments A named vector specifying the reef_siteid values and the corresponding adjustments to their carrying capacity (k). Defaults to `NULL`.
#' @param cluster_path A character string specifying the path to the folder where the existing cluster RData file is located. Used when applying fixes to an existing spatial file.
#' @param existing_cluster_file A character string specifying the name of the existing cluster RData file to which the fix will be applied (e.g., "LizardIslandCluster_allvars_dataframe.Rdata").
#' 
#' 
#' @details The function performs the following tasks:
#' \itemize{
#'   \item Reads site polygons for each cluster and checks CRS compatibility.
#'   \item Loads and projects bathymetric and wave exposure data (Ub files) to the correct CRS.
#'   \item Integrates and processes spatial data, including calculating benthic habitat pixel counts.
#'   \item Computes carrying capacity based on habitat types and performs distribution calculations.
#'   \item Adds depth information and wave exposure data to the site polygons.
#'   \item Integrates protection zone data and rearranges the data frame to match connectivity files.
#'   \item Saves processed data as shapefiles, Rdata files, and CSV files.
#' }
#' @return NULL
#' @examples
#' rootdir_data <- get_root_directory(root_dir = NULL)
#' output_directory <- file.path(rootdir_data, "ADRIA_and_IPMF/site_polygons_and_attributes")
#' connectivity_folder<- file.path(rootdir_data, "/IPMF/input/connectivity")
#' conn_files <- c("MooreCluster240404/Moore_grand_MEAN_transfer_probability_matrix_01.Rdata",
#'                 "HeronCluster240425/Heron_grand_MEAN_transfer_probability_matrix_01.Rdata",
#'                 "LizardCluster240524/Lizard_grand_MEAN_transfer_probability_matrix_01.Rdata", 
#'                 "DaviesCluster240524/Davies_grand_MEAN_transfer_probability_matrix_01.Rdata")
#' bathymetric_file <- c("SDB_AUS_CairnsCooktown_EOMAP_20180514_20181016_10m_MSL_geotiff.tif", 
#'                      "SDB_AUS_MackayCapricorn_EOMAP_20170515_20181022_10m_MSL_geotiff.tif", 
#'                      "SDB_AUS_CairnsCooktown_EOMAP_20180514_20181016_10m_MSL_geotiff.tif", 
#'                      "SDB_AUS_TownsvilleWhitsunday_EOMAP_20180603_20180903_10m_MSL_geotiff.tif")
#' bathymetric_path <- file.path(rootdir_data, "ADRIA_and_IPMF/1_spatial/GIS/Bathymetry/GBR")
#' ub_files_path <- file.path(rootdir_data, "ADRIA_and_IPMF/1_wave_exposure/Ub_data")
#' ub_files <- c("CairnsCooktown_VarWind_ubedmean_PROJ.nc.tif", 
#'               "MackayCapricorn_VarWind_ubedmean.nc_PROJ.tif", 
#'               "CairnsCooktown_VarWind_ubedmean_PROJ.nc.tif", 
#'               "TownsvilleWhitsunday_VarWind_ubedmean.nc_PROJ.tif")
#' cluster_properties <-  list(list(name = "Lizard Island Cluster", lat = c(-14, -15),long = c(145,146),use_exisitng_folder=FALSE,existing_cluster_folder_name="LizardReefCluster_2023-08-30",use_past_polygon=TRUE),
#' list(name = "Lizard Island Cluster", lat = c(-14, -15),long = c(145,146),use_exisitng_folder=FALSE,existing_cluster_folder_name="LizardReefCluster_2023-08-30",use_past_polygon=TRUE))
#' site_polygons_rdata <- "Spatial_withpolygonGeometry_PostProcessing.Rdata"
#' 
#' process_reef_clusters(connectivity_folder,conn_files, cluster_name, bathymetric_file, ub_files, previous_spatial_file, rootdir_data, output_directory, site_polygons_rdata, bathymetric_path, ub_files_path,reef_geomorphic_path,reef_geomorphic_file)
#' 
#' @authors  
#' Vanessan Haller, Anna Cresswell, Justin Moore , Pascal Omondiagbe
#' 
#' @importFrom sf st_read st_crs st_transform st_write
#' @importFrom raster raster projectRaster resample
#' @importFrom rgdal readOGR
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_wider
#' @importFrom sp CRS proj4string
#' @importFrom stringr str_detect
process_reef_clusters <- function(connectivity_folder,conn_files, cluster_properties, bathymetric_file,bathymetric_path, ub_files, ub_files_path,previous_spatial_file,rootdir_data,output_director,site_polygons_rdata,reef_geomorphic_path,reef_geomorphic_file,gbr_zones_csv,
                                  create_spatial_files = FALSE,
                                  apply_fix = FALSE, 
                                  unique_id_mappings = NULL,
                                  carrying_capacity_adjustments = NULL,
                                  cluster_path=NULL,
                                  existing_cluster_file=NULL) {
    source("01_step4_CreateSpatialFiles/AddSpatialtoSitePolygons_functions.R")
  
   
    if (missing(conn_files) || missing(cluster_properties) || missing(bathymetric_file) || missing(ub_files) || missing(previous_spatial_file)) {
      stop("Error: All input files must be provided.")
    }
  
  
    if (create_spatial_files) {
        ## Specify which reef cluster you want to add spatial information to, 'Reef Cluster of Interest'
        #Reef_of_Interest =  1 # pick 1=Moore, 2=Heron, 3=Lizard, or 4.Davies
        #i=1
      
        for (i in seq_along(cluster_properties)) {
            Reef_of_Interest = i
            cluster_name=cluster_properties[[Reef_of_Interest]]$name
            print(paste(cluster_name))
        
            # read in the site polygons for this cluster
            # Read in the site polygons for this cluster
            Polygons <- tryCatch({
              readRDS(file.path(output_directory, previous_spatial_file[Reef_of_Interest], site_polygons_rdata))
            }, error = function(e) {
              stop(paste("Error reading polygons file:", e))
            })
            
            #the _PostProcessing is from Vanessas code which does the final rule-based oining of polygons into multipolygons etc - Note (to check with Vanessa)
            Polygons = readRDS(paste(output_directory, previous_spatial_file[Reef_of_Interest], site_polygons_rdata, sep="/")) # CHECK - we should consider just working with geopackage. which would be st_read I think instead of readRDS
            
            
            cluster_folder = paste(output_directory, previous_spatial_file[Reef_of_Interest], sep ="/")
           
             # choose the appropriate bathymetry file:
            bathy_file = file.path(bathymetric_path,bathymetric_file[Reef_of_Interest])
            bathy_raster <- raster(bathy_file)
            
           
            ub_file_info <- ub_files[[Reef_of_Interest]]
            ub_raster = raster(file.path(ub_files_path, ub_file_info$path))
            crs <-  ub_file_info$crs
            
            
            #Files are direct download from SWAN website
            # I had   issue with the .tif in that it didn’t seem to have any spatial information. 
            # I loaded the netcfd version into QGIS, I changed the CRS to e.g. WGS84 / UTM zone 55S and it put it in the right place.
            
            # The netcdf’s are 80MB so QGIS didn’t love it but it did load eventually.
            #Mariana seems to be doing it in R but couldn't get it to work, so create version in QGIS and save with "_PROJ at the end" into:
          
            
            
            # Project the raster if a CRS is determined
            # if (!is.null(crs)) {
            #   ub_raster <- projectRaster(ub_raster, crs = crs)
            # } else {
            #   warning(paste("No CRS found for file:", ub_files[i]))
            # }
            # 
            #sf::st_crs(Polygons)$proj4string != 
            if (sp::proj4string(bathy_raster) != sp::proj4string(ub_raster)) {
              stop("Error: CRS of input files do not match.")
            }
            
            
            # Test plot to check if files overlap
            
            # #plotting might take some time, so we should allow some flexibility
            # ggplot() +
            #   geom_sf(data = Polygons, fill = NA, color = 'blue') +
            #   geom_raster(data = as.data.frame(as(bathy_raster, "SpatialPixelsDataFrame")), aes(x = x, y = y, fill = layer), alpha = 0.5) +
            #   geom_raster(data = as.data.frame(as(ub_raster, "SpatialPixelsDataFrame")), aes(x = x, y = y, fill = layer), alpha = 0.5) +
            #   labs(title = paste("Test Map for", cluster_name[Reef_of_Interest])) +
            #   theme_minimal()
            
            print(paste("CRS check passed for", cluster_name))
            
            
            # extract the unique IDs for each reef and its name
            unique_reefs = as.data.frame(Polygons) %>%
              dplyr::select(UNIQUE_ID, Reef) %>%
              unique()
            
            print(unique_reefs$Reef)
            GBR_IDs = unique(unique_reefs$UNIQUE_ID)
            GBR_Names = unique(unique_reefs$Reef)
            print(paste("Number of reefs:", length(GBR_IDs)))
            
            # cluster
            
            # Pull information from the site polygon file
            sites <- Polygons[, c("site_id", "Reef", "habitat", "area", "UNIQUE_ID", "geometry")]
            names(sites)[1] <- "site_id"
            names(attributes(sites)$agr)[1] <- "site_id"
            sites$site_id <- as.character(sites$site_id)
            attributes(sites)$row.names <- sites$site_id
            sites$site_id <- as.factor(sites$site_id)
            
            #####################################################
            ##### Add benthic habitat pixels count columns ######
            #####################################################
            print("adding benthic habitat pixels count columns")
            sites_withBenthic <- list()
            for (ID in GBR_IDs){
              print(GBR_Names[which(GBR_IDs==ID)])
              sites_reef <- sites[sites$UNIQUE_ID == ID,]
              #plot(sites_reef) # visualise for a quick check
              if (nrow(sites_reef) > 0){
                sites_withBenthic[[which(GBR_IDs==ID)]] <- CalculateBenthic(sites_reef, outdir=cluster_folder,reef_geomorphic_path,reef_geomorphic_file)
              } else {
                sites_withBenthic[[which(GBR_IDs==ID)]] <- NULL
              }
            }
            sites_all = do.call(rbind, sites_withBenthic)
            
            ###############################################
            ######## Calculate carrying capacity ##########
            ###############################################
            print("Calculate carrying capacity")
           
            ## Calculate carrying capacity original ####
            sites_all$k = ((sites_all$rubble*0.125 + sites_all$sand*0.125 + sites_all$rock*0.63 + sites_all$coral_algae*0.63)/(sites_all$total_pixel-sites_all$na_pixel))*100 # ignore NAs here
            
            # !!! AKC in the final spatial files for Counterfactual 2025 we changed this to the following numbers:
            # spatial$k = ((spatial$rubble*0.1 + spatial$sand*0.1 + spatial$rock*0.9 + spatial$coral_algae*0.9)/(spatial$total_pixel-spatial$na_pixel))*100
            
            
            # there's still some cells with no information - case when no adjacent site within reef satisfying conditions.
            # give these a mean across sites_all
            sites_all$Surrogate[which(is.na(sites_all$k))] <- "none_found"
            sites_all$k[which(is.na(sites_all$k))] <- mean(sites_all$k, na.rm = T)
            
            ## Calculate carrying capacity distribution ####
            print("Calculate carrying capacity distribution")
            for (r in 1:nrow(sites_all)) {
              rub = runif(sites_all$rubble[r], 0, 0.25)
              if (length(rub) == 0) rub = 0
              sand = runif(sites_all$sand[r], 0, 0.25)
              if (length(sand) == 0) sand = 0
              rock = runif(sites_all$rock[r], 0.26, 1)
              if (length(rock) == 0) rock = 0
              coral_algae = runif(sites_all$coral_algae[r], 0.26, 1)
              if (length(coral_algae) == 0) coral_algae = 0
              sites_all$k_distribution[r] =  (sum(rub, sand, rock, coral_algae) /
                                                (sites_all$total_pixel[r]-sites_all$na_pixel[r]))*100
            }
            sites_all$k_distribution[which(is.na(sites_all$k_distribution))] <- mean(sites_all$k_distribution[!(is.na(sites_all$k_distribution))])
            
            summary(sites_all$k)
            summary(sites_all$k_distribution)
        
            #### Calculate carrying capacity according to Y-M ####
            
            # AKC CHECK - needs revisiting
            
            # mypalette<-c(brewer.pal(8, "Set1")[2:8])
            # Habitats<-levels(sites_all$habitat)
            # sites_all$k<-NA # create a column for carrying capacity 
            # 
            # 
            # # AKC _ I can't remember what is going on here, I don't think this was resolved with YM? Vanessa you found an error and then I'm not sure what you ended up deciding was the best path??
            # a<-sum(sites_all$rubble)/sum(sum(sites_all$rock),sum(sites_all$coral_algae))
            # b<-sum(sites_all$sand)/sum(sum(sites_all$rock),sum(sites_all$coral_algae))  
            # c<-sum(sum(sites_all$sand),sum(sites_all$rubble))/sum(sites_all$rock)  
            # d<-sum(sum(sites_all$sand),sum(sites_all$rubble))/sum(sites_all$coral_algae) 
            # 
            # a<-sum(sites_all$rubble)/sum(sites_all$total_pixel)
            # b<-sum(sites_all$sand)/sum(sites_all$total_pixel)  
            # c<-sum(sum(sites_all$sand),sum(sites_all$rubble))/sum(sites_all$total_pixel)   
            # d<-sum(sum(sites_all$sand),sum(sites_all$rubble))/sum(sites_all$total_pixel)   
            # # this is overwriting the above 
            # a = 0.056 # average ratio of rubble over (rock + coral_algae)
            # b = 0.111 # average ratio of sand over (rock + coral_algae)
            # c = 0.285 # average ratio of (sand+rubble) over rock
            # d = 0.402
            # # this is overwriting the above 
            # 
            # for (i in Habitats){
            #   sites_all$k[sites_all$habitat==i]<-CalculateK_YM(sites_Habitat=sites_all[sites_all$habitat==i,])
            # }
            
            ####################################
            #### add depth #####################
            ####################################
            sites_all <- AddDepth(sites_all, bathy_file) 
            
            ##################################################
            #### Add wave exposure, Ub #######################
            ##################################################
            ubtif= ub_file_info$path
            sites_all <- AddUb(sites_all, ubtif,ub_files_path)
            sites_all$Ub = sites_all$ub_med #use the median Ub in the model for now, but could choose mean here if you want
            
            #are there very large / small values here?? should set them to NA, may need to do same code as justin made for this, i.e. search for nearest polygon if no information?
            
            #################################################
            #### add protection zones #######################
            #################################################
            if (!is.null(gbr_zones_csv)) {
              gbr_zones <- read.csv(gbr_zones_csv) %>%
                mutate(UNIQUE_ID = as.factor(UNIQUE_ID))
              
              sites_all <- sites_all %>%
                left_join(gbr_zones, by = "UNIQUE_ID")
            }
            
            # Remove spaces in the reef_siteid column if it exists
            if ("reef_siteid" %in% colnames(sites_all)) {
              sites_all <- sites_all %>%
                mutate(reef_siteid = gsub(" ", "", reef_siteid))
            }
            
            
            ##################################################
            #### Rearrange dataframe #####################
            ##################################################
            
            combined1 = sites_all %>%
              relocate(reef_siteid, .before = habitat) %>%
              relocate(UNIQUE_ID, .before = site_id) %>%
              relocate(Reef, .before = site_id) %>%
              relocate(reef_siteid, .before = site_id) %>%
              dplyr::select(-ID, rubble_raw,sand_raw,rock_raw,coral_algae_raw,na_pixel_raw,total_pixel_raw) %>% #removing these raw columns for now. but they could be useful if want to approach what to do when lots of NAs in a different ways
              relocate(benthic_mode, .after = ZONE_TYPE) %>%
              relocate(rubble, .after = ZONE_TYPE) %>%
              relocate(sand, .after = ZONE_TYPE) %>%
              relocate(rock, .after = ZONE_TYPE) %>%
              relocate(coral_algae, .after = ZONE_TYPE) %>%
              relocate(na_pixel, .after = ZONE_TYPE) %>%
              relocate(total_pixel, .after = ZONE_TYPE) %>%
              mutate(sf_area = area) %>%
              mutate(area = sf_area*ratio2d3d) %>% # this is ratio from bathymap (anna version)
              dplyr::select(-site_id) #not required, it is the same as reef_siteid
            
            names(combined1)
            
            
            #Add 2d3d area ratio from Roelfsema
            #setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/Documents/GitHub/C_scape_Input/1_initialise_reef_spatiotemporal")
            ratio_join = read.csv("01_step4_initialise_reef_spatiotemporal/Roelfsema2021_2d3d_ratio.csv") %>%
              dplyr::select(habitat, Region, Ratio) %>%
              mutate(habitat = recode(habitat,
                                      "Reef Crest" = "Crest",
                                      "Outer Reef Flat" = "Outer Flat",
                                      "Sheltered Reef Slope" = "Sheltered Slope",
                                      "Reef Slope" = "Slope"))
            
            if (i %in% c(1,3)) ratio_join = ratio_join %>% filter(Region == "Cairns/Cooktown") 
            if (i == 2) ratio_join = ratio_join %>% filter(Region == "Mackay/Capricorn") 
            if (i %in% c(4)) ratio_join = ratio_join %>% filter(Region == "Townsville/Whitsundays") 
            
            ratio_join = ratio_join %>%dplyr::select(-Region)
            # pick 1=Moore, 2=Heron, 3=Lizard, or 4.Davies
            
            combined1 = combined1 %>%
              left_join(ratio_join, by = "habitat") %>%
              mutate(area = ifelse(is.na(area), sf_area, area))
              #mutate(area3D_roelf = sf_area*Ratio)
            
            # Check for NAs in the area column and print a warning
            na_count <- sum(is.na(combined1$area))
            if (na_count > 0) {
              warning(paste("There are", na_count, "NA values in the 'area' column after the join."))
            }
            
            #connectivitity files 
            conn_site_order = rownames(readRDS(file.path(connectivity_folder,conn_files[Reef_of_Interest]))) #order of connectivity matrix
            
            # Get the order of rows based on conn_site_order
            order <- match(conn_site_order, combined1$reef_siteid)
            
            combined1 = combined1[order,]
            
            cluster_save = gsub(" ", "", cluster_name)
            
            ##creates new directory to save varables into within the site polygons folder for each cluster and version
            new_output_folder <- paste(cluster_folder, "/", gsub(" ","",cluster_save[Reef_of_Interest]),"_Spatial-Variables", Sys.Date(), sep = "")
            
            #create folder
            dir.create(new_output_folder)
            #create maps folder within new file for grpahica outputs of this script
            dir.create(paste(new_output_folder,"maps", sep = "/"))
            
            
            #save shapefile
            # Define the path to the output file
            output_file <- file.path(new_output_folder,  paste0(cluster_save[Reef_of_Interest], "_SpatialGeometry_allvars.shp"))   
            
            
            # Check if the file already exists
            if (file.exists(output_file)) {
              message("The file already exists at ", output_file, ". Skipping file write.")
            }else{
              # Write the spatial data to the file
              sf::st_write(combined1, output_file)
              
            }
            
            
            
            # save Rdata file with point info only----
            # Define the path to the output RData file
            rdata_file <- file.path(new_output_folder,  paste0(cluster_save[Reef_of_Interest], "_SpatialPoint_allvars.Rdata"))
            
            # Check if the RData file already exists
            if (file.exists(rdata_file)) {
              message("The RData file already exists at ", rdata_file, ". Skipping file save.")
            }else{
              cluster_point <- combined1 %>%
                st_centroid()
              
              # Save the RData file
              saveRDS(object = cluster_point, file = rdata_file)
              
            }
            
           
            # save csv dataframe file with lat long columns----
            cluster_df = cluster_point %>%
              mutate(long = unlist(map(cluster_point$geometry,1)),
                     lat = unlist(map(cluster_point$geometry,2)))
            
            cluster_csv = as.data.frame(cluster_df) %>%
              dplyr::select(-geometry)
            
            #comment write.csv(cluster_csv, paste0(new_output_folder,"/",cluster_save[Reef_of_Interest], "_SpatialPoint_allvars.csv"), row.names = F) # probably not needed but was previously being given to connectivity  / ADRIA (now using geopackage?)
            
            saveRDS(object = cluster_csv, file = paste0(new_output_folder,"/",cluster_save[Reef_of_Interest], "_allvars_dataframe.Rdata")) #this used by C~scape model?
            #AKC CHECK - we need to reassess which files we actually want/ need to avoid heaps of files
            
            
            
            # save geopackage ----
            st_crs(combined1)
            
            #setwd("C:/Users/acresswe/Australian Institute of Marine Science/RRAP M&DS - 2_DataWorkflows/ADRIA_and_IPMF/site_polygons_and_attributes")
            #had issue with too long file path for geopackage so saving in main folder
            #comment st_write(combined1, paste0(cluster_save[Reef_of_Interest], "_SpatialGeometry_allvars.gpkg"), append = TRUE)
            # Define the path to the output GeoPackage file
            gpkg_file <- file.path(new_output_folder,  paste0(cluster_save[Reef_of_Interest], "_SpatialGeometry_allvars.gpkg"))     
            
            # Check if the GeoPackage file already exists
            if (file.exists(gpkg_file)) {
              message("The GeoPackage file already exists at ", gpkg_file, ". Skipping file write.")
              next # Skip to the next iteration of the loop
            }
            sf::st_write(combined1, gpkg_file, append = TRUE)
            ## save any information on this file creation as Readme
            # Define the content of the README
            readme_content <- "
            # Made by Anna Cresswell
            # on 5 6 2024
            # 3D area calculation is A3D = A2d/cos(slope)
            "
            
            # Write the README content to a file
            #comment writeLines(readme_content, paste0(new_output_folder,"/",cluster_save[Reef_of_Interest], "README.md"))
        }
  
    }
  
    # Check if the fix for a particulrar Island or other reefs needs to be applied
    if (apply_fix && (!is.null(unique_id_mappings) || !is.null(carrying_capacity_adjustments))) {
      
      # Ensure the data file is provided
      if (is.null(cluster_path) && is.null(existing_cluster_file)) {
        stop("Error: You must provide the the cluster data file (e.g. lizard_data_file) when apply_fix is TRUE.")
      }
      
      
      # Read the Rdata version of the output
      clusterdat <- readRDS(paste0(cluster_path, existing_cluster_file))
      
      # Apply the necessary UNIQUE_ID fixes using unique_id_mappings
      if (!is.null(unique_id_mappings)) {
        clusterdat <- clusterdat %>%
          mutate(UNIQUE_ID = case_when(
            UNIQUE_ID %in% names(unique_id_mappings) ~ unique_id_mappings[UNIQUE_ID],
            TRUE ~ UNIQUE_ID  # Keep other IDs unchanged
          ))
      }
      
      # Apply the carrying capacity adjustments if specified
      if (!is.null(carrying_capacity_adjustments)) {
        clusterdat <- clusterdat %>%
          mutate(k = ifelse(reef_siteid %in% names(carrying_capacity_adjustments),
                            carrying_capacity_adjustments[reef_siteid],
                            k))  # Keep the original 'k' if no adjustment
      }
      
  
      # Append current date to the file name
      new_file_name <- sub("\\.Rdata$", paste0("_", Sys.Date(), ".Rdata"), existing_cluster_file)
      
      # Save the modified lizarddat if needed
      saveRDS(object = clusterdat, file = paste0(cluster_path, new_file_name))
      
      message("Fix has been applied to the specified UNIQUE_IDs and carrying capacities.")
    }
  
}




# ##################################
# ###########PLOTS##################
# ##################################
# 
# spatial_plot = combined1 %>%
#   mutate(k_bin = ifelse(k<20, "<20", 
#                         ifelse(k>20&k<30, "20-30", 
#                                ifelse(k>30&k<40, "30-40", 
#                                       ifelse(k>40&k<50, "40-50", 
#                                              ifelse(k>50&k<60, "50-60", 
#                                                     "60-67"))))))
# 
# #continuous map of carrying capacity
# ggplot() + 
#   geom_sf(data = spatial_plot, aes(fill = k), colour = "black") +
#   scale_fill_viridis(direction = 1, name = "Coral carrying\ncapacity (%)", limits = c(0,max(spatial_plot$k))) + #
#   labs(title = cluster_name[Reef_of_Interest]) +
#   annastheme +
#   scale_x_continuous(breaks = c(146.18, 146.24, 146.30))
# 
# ggsave(filename = paste(new_output_folder,"/maps/CarryingCapacity_continuous.png",sep=""), width = 8, height = 7)
# 
# 
# #continuous map of wave exposure
# ggplot() + 
#   geom_sf(data = spatial_plot, aes(fill = ub_med), colour = "black") +
#   scale_fill_viridis(direction = 1, name = "Ubed_mean\n('ub_med')", limits = c(0,max(spatial_plot$ub_med))) + #
#   labs(title = cluster_name[Reef_of_Interest]) +
#   annastheme +
#   scale_x_continuous(breaks = c(146.18, 146.24, 146.30))
# 
# ggsave(filename = paste(new_output_folder,"/maps/Waves.png",sep=""), width = 8, height = 7)
# 
# 
# #depth ----
# spatial_plot = spatial_plot %>%
#   mutate(depth_cat = ifelse(depth_med <12, "S", "D"))
#          
# ggplot() + 
#   geom_sf(data = spatial_plot, aes(fill = depth_cat), colour = "black") +
#   #scale_fill_viridis(direction = 1, name = "Ubed_mean\n('ub_med')", limits = c(0,max(spatial_plot$ub_med))) + #
#   labs(title = cluster_name[Reef_of_Interest]) +
#   annastheme +
#   scale_x_continuous(breaks = c(146.18, 146.24, 146.30))
# 
# ggsave(filename = paste(new_output_folder,"/maps/Depth_Category.png",sep=""), width = 8, height = 7)
# 
# 
# 
# 
# 
# 
# #categorical map of carrying capacity
# spatial_plot$k_bin = factor(spatial_plot$k_bin, levels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-67"))
# 
# ggplot() + 
#   geom_sf(data = subset(spatial_plot, !(is.na(k_bin))), aes(fill = k_bin), colour = "black") +
#   scale_fill_manual(values = viridis(6)) +
#   labs(title = cluster_name[Reef_of_Interest]) +
#   annastheme +
#   scale_x_continuous(breaks = c(146.18, 146.24, 146.30))
# 
# ggsave(filename = paste(new_output_folder,"/maps/CarryingCapacity_categorical.png",sep="") , width = 8, height = 7)
