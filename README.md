
# 1_step4_CreateSitePolygons Folder

## Purpose
This folder contains scripts for generating site polygons, which are spatial units required for modeling reefs.

## To Run

Edit the parameters in `01_Run_SitePolygons.R`, and then run the file.


## Key Files

###  `01_Run_CreateSitePolygons.R` 

- **Function:** `create_site_polygons`

  This script partitions reef(s) into 'site polygons', which are spatial units for the model of mean size approximately 300 m in diameter. This includes functions to pixelate the reef and to cluster these pixels together and the post-processing which checks multi-polygons and for too-small polygons. It creates a folder and saves the output to this folder.
  

#### Inputs:

1) **GBR_shape_file**  
   This file contains outlines of all reefs on the Great Barrier Reef. The Unique IDs in this shapefile are used across all RRAP Modelling and Decision Support to distinguish between reefs. The user needs to specify which reefs (via Unique IDs) are of interest. It is passed as a  character string specifying the path to the shapefile for the Great Barrier Reef. This file contains the spatial outlines of all reefs on the GBR, and its Unique IDs are used across all RRAP Modelling and Decision Support.

2) **reef_file: Geomorphic Zonation Map**  
    A character string specifying the name of the benthic habitat raster file (.tif) to be used for extracting habitat data. This file provides essential habitat information for the reefs. The geographic boundaries of the reef area in the chosen location are determined from a publicly available (GBRMPA 2021) geomorphic zonation map that depicts the distribution of geomorphic features such as reef crest, slopes, reef flats, and lagoons (Roelfsema et al. 2021c). This map extends to a depth of approximately 20 m, setting the depth constraints of our current modelling efforts. You can access the geomorphic zonation map at the following links:  
   - [GBRMPA Geomorphic Zonation Map (Roelfsema et al. 2021c)](https://www.arcgis.com/home/item.html?id=bdfb4cee7a484140a32c615145165dbf)
   - [Reef Knowledge System - Reef Geohub](https://reefknowledgesystem.gbrmpa.gov.au/maps/reef-geohub)

   Following recommendations by Kennedy et al. (2021), only the map classes expected to have predominantly hard substrates (Reef Slope, Reef Crest, Outer Reef Flat, and Sheltered Reef Slope) are considered as zones where coral growth is likely.

3) **output_directory**  
   A character string specifying the directory where the output files and site polygons will be saved.

4) **cluster_properties**  
   A list containing properties for each reef cluster, such as the cluster name, latitude, longitude, and other relevant parameters. These parameters define the location and characteristics of the clusters being used in the model.

5) **bathymetric_file**  
   A character string or vector specifying the name(s) of the bathymetric file(s) to be used in creating the site polygons. Bathymetric data is essential for depth-related characteristics of the site polygons.  A bathymetric map of the area is required. We currently source ours from [EOMAP](https://www.eomap.com/).It is used to calculate depth-related characteristics.

6) **bathymetric_path**  
   A character string specifying the directory path where the bathymetric files are located.

7) **GBR_IDs**  
   A vector of unique IDs representing the Great Barrier Reef sites to be included in the polygon creation process. These IDs are crucial for identifying the reefs of interest.


8) **reefs_path**  
   A character string specifying the directory path where the benthic habitat raster file is located. This path directs the script to the location of the raster file needed for habitat analysis.

9) **site_size**  
   A numeric value specifying the desired size of the site polygons, typically in square meters. This controls the target size of each polygon unit created.

10) **reshex**  
    A character string specifying the resolution of the hexagons to be used in the spatial analysis. This parameter controls the spatial precision of the H3 hexagonal grid system.

11) **resolution**  
    An integer specifying the resolution level for the hexagons in the H3 grid system. This influences how the geomorphic zones are divided into hexagons for spatial analysis.

12) **unit**  
    A character string specifying the unit of measurement for the hexagon area (e.g., "m^2" for square meters). This defines the spatial units used in the model.


   First, geomorphic zones are pixelated into hexagonal cells using the geospatial indexing system H3 in the package `h3` (Kuethe 2021) at resolution 12 (approximately 20 m diameter). The depth of each pixel is determined.

   Graph theory is used to establish vertices (lines between points) between the hexagons within a geomorphic zone of a given reef. In particular, Delaunay triangulation is used to find all triangles within the graph. Triangulation is chosen instead of nearest-neighbors or distance-based neighbor algorithms because it helps bridge the graph between distant areas of the reef. Due to splitting reefs into geomorphic zones, the hexagons are not contiguous, which means that other neighborhood identification methods would result in non-connected graphs.

   Once all possible vertices are established using the previous step, each vertex is assigned a weight (cost of making that connection) that is based on physical distance and the difference in depth assigned to the two hexagons. The weight is calculated using a Euclidean distance for the normalized vertex length and depth difference. 

Finally, the minimum spanning tree is calculated using these weights and the `mst` function within the `sfnetworks` package in R. The minimum spanning tree represents the single path that connects all hexagons with the smallest cost as calculated by the weights for each vertex used to create the path. Several files are saved with different file extensions. We are moving towards working only with the `geopackage` format but currently also have `Rdata` and `CSV` files.

#### Clustering and Post-Processing:

Once the minimum spanning tree is created, we can cluster the hexagons using SNP-based kinship analysis within the `skater` package for R. The `skater` function uses the depth for each hexagon to decide on the best splits along the path of the minimum spanning tree. The methods are described in detail in Turner et al. (2021). The number of clusters is not fixed and depends on hexagon characteristics (how homogeneous depths are between hexagons) and the number of hexagons in the network. We pass in a minimum size for a site polygon that represents 200 hexagons (~61,400 m²), just below the RECOM grid size of 62,500 m². Due to the setup of the algorithm and the splitting in geomorphic zones, some site polygons might form multi-polygons.

A **multi-polygon** refers to a single site polygon that has a spatial break, meaning it may be made up of discontinuous patches of reef. In post-processing, we assess whether these multi-polygons should remain one multi-polygon or be split into several polygons depending on the distance between the polygons and the size of each polygon.

During post-processing, we start by splitting each multi-polygon into the separate polygons, then we use the following rules to recombine polygons:

1. Identify the largest polygon.
2. Calculate the nearest distance using `st_distance` between boundary points of all polygons.
3. If any of the other polygons are within 100 m, merge them with the largest polygon as a multi-polygon. If no polygon is within 100 m, then the largest polygon remains alone.
4. Remove the polygons used combined in step 3 from the polygon list.
5. Repeat steps 1–4 until all polygons are grouped.

Finally, post-processing also ensures that we do not have site polygons that are too small. Hence, all polygons (or multi-polygons) that are smaller than 50 hexagons (~15,350 m²) are removed. 
The script uses hexagonal pixels and graph theory to create the polygons. These polygons are then clustered based on depth and geomorphic characteristics.

#### Output Files:

- **Geopackages**  
- **Rdata Files**  
- **CSV Files**  

While we are moving towards working primarily with geopackages, the current workflow includes the generation of other file formats like `Rdata` and `CSV`.

### `Run_AddSpatialtoSitePolygons.R`
This script adds additional columns to the files creating by Run_CreateSitePolygons.R. It reads in these files from where they have been saved, and then used additional spatial maps to source additional information including depth, wave exposure and benthic composition of the site polygons that were created in Run_CreateSitePolygons.R. 

Carrying capacity for each site was calculated from benthic habitat maps (Roelfsema et al. 2021c). Each 10 m pixel in the benthic map was classified as one of four categories: Sand, Rubble, Rock, Coral/Algae.

Site polygons created in Run_CreateSitePolygons.R are overlaid on the benthic map and the number of pixels of each of the four categories, as well as the number of pixels than are NA, and the total number of pixels per site polygon.

In cases where a site polygon had more than 95% of the pixel as NA values, the code will search for the nearest adjacent polygon in the same geomorphic zone, and was assigned it's benthic value counts.

Potential coral habitat per site is calculated from these counts. According to Roelfsema, et al. 2021, pixels defined as dominated by Coral/Algae and Rock can be considered as potential coral habitat with Coral/Algae being “any hardbottom area supporting living coral and/or algae” and Rock being “any exposed area of hard bare substrate”. As such, pixels classified to be dominant in either Coral/Algae or Rock were counted as suitable coral habitat. However, pixels are 10 x 10 m in size, and, due to the patchy nature of reefs at very fine scales, it is unlikely that the full area of the pixel is completely covered by the dominant classification. For one of the four possible categories to be dominant, the dominant category would need to cover anywhere between 26-100% of the pixel (average 63%), while the non-dominant categories would be ≤25% (average 12.5%). The mean coral habitat was calculated by considering the number of pixels of each type and these averages: 
percentage_coral_habitat = (rubble_pixels*0.125 + sand_pixels*0.125 + rock_pixels*0.63 + coral_algae_pixels*0.63)/(total_pixels-na_pixels))*100

The site polygons are also overlaid on a bathymetric map and a map of wave exposure (ubedmean from the SWAN model https://espace.library.uq.edu.au/view/UQ:8246441) in .tif format. 

From this sites are assigned a medium depth and wave exposure. 
It is important these .tif files are projected correctly.
Far North: EPSG:32754  WGS 84  UTM zone 54S (No current Cscape clusters)
Townsville-Whitsunday: EPSG:32755  WGS 84  UTM zone 55S (Cscape Davies Reef Cluster)
Cairns-Cooktown: EPSG:32755  WGS 84  UTM zone 55S (Cscape Moore Reef Cluster and Lizard Island Cluster*)
Mackay-Capricorn: EPSG:32756  WGS 84  UTM zone 56S (Cscape Heron Island Cluster*)
* Anna has checked Spatial File geopackage and .tif line up)

The wave exposure estimates were generated by Callaghan and colleagues (Callaghan et al., 2015) using the Simulating Waves Nearshore (SWAN) model, which performs numerical modelling to compute wind-generated waves hourly over a long time series wind data at a 10 km spatial resolution, depending on small-scale bathymetry. There are different wave variables and the resolution is at a 10x10m scale. Each variable was summarised with various statistics (e.g., 70 is the 70th quantile, mean, 100 is the maximum, etc.). Ubedmean was chosen as the best representative from the SWAN model for our purposes here. Ubedmean is the mean horizontal velocity at the seafloor, and gives an indication of how much force water movement at the seafloor is exerting on corals, influencing their growth and survival via physical impact and through the transfer of nutrients from the water to the corals.  Hs (significant wave height) is another option that has the benefit of being available more globally, but ubedmean is generally considered the better option if it is available. 

- **Function:** `process_reef_clusters`

The function performs the following tasks:

- Reads site polygons for each cluster and checks CRS (Coordinate Reference System) compatibility.
- Loads and projects bathymetric and wave exposure data (Ub files) to the correct CRS.
- Integrates and processes spatial data, including calculating benthic habitat pixel counts.
- Computes carrying capacity based on habitat types and performs distribution calculations.
- Adds depth information and wave exposure data to the site polygons.
- Integrates protection zone data and rearranges the data frame to match connectivity files.
- Saves processed data as shapefiles, Rdata files, and CSV files.

  
#### Input Parameters:

- **`conn_files`**: A character vector containing the paths to connectivity files. These files are used to rearrange the spatial file to match the order of sites.
- **`cluster_properties`**: A list containing the names of the clusters being processed and their respective properties.
1) **`bathymetric_file`**: A character vector containing the paths to bathymetric files that need to be projected to the correct coordinate system.
2) **`ub_files`**: A character vector containing the paths to Ub files required for wave exposure calculations.
3) **`previous_spatial_file`**: A character vector containing the path to the previous spatial file.
4) **`rootdir_data`**: A character string specifying the root directory for data.
5) **`output_directory`**: A character string specifying the directory where output files will be saved.
6) **`site_polygons_rdata`**: A character string specifying the path to the site polygons `.Rdata` file.
7)**`bathymetric_path`**: A character string specifying the path to the bathymetric files directory.
8) **`ub_files_path`**: A character string specifying the path to the Ub files directory.
9) **`connectivity_folder`**: A character string specifying the path to the connectivity folder.
10) **`reef_file`**: A character string specifying the reefs file.
11) **`reefs_path`**: A character string specifying the path to the reefs file directory.
12) **`gbr_zones_csv`**: A full path to the protection zone CSV file.
13) **`create_spatial_files`**: Logical, whether to create new spatial files (e.g., shapefiles, RData, CSV). Set to `TRUE` to create files or `FALSE` to skip file creation and apply only fixes. Defaults to `FALSE`.
14) **`apply_fix`**: Logical, whether to apply fixes to an existing spatial file (e.g., `UNIQUE_ID` or carrying capacity adjustments). Defaults to `FALSE`.
15) **`unique_id_mappings`**: A named vector specifying mappings of old `UNIQUE_ID`s to new ones. Use this to adjust `UNIQUE_ID` values in the spatial data. Defaults to `NULL`.
16) **`carrying_capacity_adjustments`**: A named vector specifying `reef_siteid` values and corresponding adjustments to their carrying capacity (`k`). Defaults to `NULL`.
17) **`cluster_path`**: A character string for the path to the folder where the existing cluster RData file is located. Required when applying fixes.
18) **`existing_cluster_file`**: A character string for the name of the existing cluster RData file to which the fix will be applied (e.g., `LizardIslandCluster_allvars_dataframe.Rdata`).


- **Outputs**: shapefiles, Rdata files, and CSV files.

Unique IDs are used across all RRAP Modelling and Decision Support.
   
   

### `AddSpatialtoSitePolygons_functions.R`
This script contains the necessary functions to support the Run_AddSpatialtoSitePolygons.R script. These functions are used to calculate and analyze pixel categories within site polygons and to retrieve data from neighboring polygons in cases where there is missing information within a polygon. The provided functions enable detailed integration and processing of spatial data related to reef sites, handling various data types such as benthic habitat, depth, area, and wave exposure. These metrics are essential for further reef modeling and analysis.

The code contain the following functions :


#### 1. `CalculateBenthic()`
- **Purpose**: This function calculates the composition of benthic habitats for each reef site based on a benthic habitat raster map. It extracts pixel-based habitat data, calculates habitat metrics, and handles missing data by interpolating from the nearest available sites.
- **Parameters**:
  - `sites_reef`: An `sf` object containing spatial data for reef sites.
  - `outdir`: A character string specifying the output directory where results will be saved (not used in the function but reserved for future use).
  - `reefs_path`: A character string specifying the directory where the benthic habitat raster file is located.
  - `reef_file`: A character string specifying the name of the benthic habitat raster file (.tif).
- **Details**:
  - Reads the benthic habitat raster file.
  - Extracts benthic habitat data for each site.
  - Calculates the amount of each benthic habitat type (rubble, sand, rock, coral_algae) based on pixel counts.
  - Handles missing data by interpolating values from the nearest sites with available data.
  - Updates the spatial dataset with calculated habitat metrics.
  - Adds area information for each site and creates an ID column.
  - Uses Euclidean distance to replace missing values with data from the nearest site.
- **Output**: An `sf` object with updated habitat metrics, including the amount of rubble, sand, rock, coral algae, NA pixels, and total pixels.
  
#### 2. `AddDepth()`
- **Purpose**: This function integrates bathymetric data into the spatial dataset, adding depth and 3D area information. It ensures that the bathymetric raster and site data share the same coordinate reference system (CRS) and calculates additional metrics such as slope and area ratios.
- **Parameters**:
  - `sites_all`: An `sf` object containing spatial data for the sites.
  - `bathy_file`: A character string specifying the path to the bathymetric raster file (.tif).
- **Details**:
  - Reads the bathymetric raster file and processes it.
  - Transforms the CRS of the site data if necessary.
  - Crops the bathymetric raster to match the extent of the sites.
  - Computes slope from the bathymetric data.
  - Extracts depth values and computes statistics (mean, standard deviation, median).
  - Handles missing depth data by interpolating from the nearest neighbors.
  - Joins depth and area information to the spatial dataset.
- **Output**: An `sf` object with additional columns for depth and area metrics (`depth_mean`, `depth_sd`, `depth_med`, `area2d`, `area3d`, `ratio2d3d`).

#### 3. `AddUb()`
- **Purpose**: This function integrates wave exposure data into the spatial dataset by adding wave exposure metrics. It processes a raster file containing wave data and extracts relevant values for each site, handling missing data by interpolating from the nearest non-missing values.
- **Parameters**:
  - `sites_all`: An `sf` object containing spatial data for the sites.
  - `Ubtif`: A character string specifying the name of the wave exposure raster file (.tif).
  - `ub_files_path`: A character string specifying the directory containing the wave exposure raster files.
- **Details**:
  - Reads and processes the wave exposure raster file.
  - Transforms the CRS of the sites data to match the raster.
  - Extracts wave exposure values for each site.
  - Filters out negative values and calculates wave exposure statistics (mean, standard deviation, median).
  - Handles missing wave exposure data by interpolating from the nearest non-missing values.
- **Output**: An `sf` object with additional columns for wave exposure metrics (`ub_mean`, `ub_sd`, `ub_med`).

#### 4. `CalculateK_YM()`
- **Purpose**: This function calculates the carrying capacity of the reef sites based on their benthic composition, with specific parameters for non-colonizable areas like sand and rubble.
- **Parameters**:
  - `sites_Habitat`: An `sf` object containing habitat data for reef sites.
- **Details**:
  - Uses the proportions of benthic categories like sand, rubble, rock, and coral algae to calculate the carrying capacity.
  - The carrying capacity is calculated by adjusting for non-colonizable areas.
- **Output**: Returns the carrying capacity for each reef site.


###  `00_Create_Add_Site_polygon.R` 

- **Function:** `Create_add_site_polygon`

This function, `Create_add_site_polygons()`, allows users to either create new site polygons or add spatial data to existing polygons. The function provides flexibility for generating and updating spatial data based on connectivity, bathymetric, and other datasets. Depending on the user's input, the function can either create new polygons or enhance existing ones by adding spatial data such as bathymetric information or connectivity properties.
  

#### Inputs:

- **connectivity_folder**: A character string specifying the directory path where the connectivity files are located.
- **conn_files**: A character string or vector specifying the names of the connectivity files to be used.
- **cluster_properties**: An object containing properties related to reef clusters. This object is used in the creation or processing of site polygons.
- **bathymetric_file**: A character string specifying the name of the bathymetric file to be used.
- **bathymetric_path**: A character string specifying the directory path where the bathymetric file is located.
- **ub_files**: A character string or vector specifying the names of the underlying bathymetric files to be used.
- **ub_files_path**: A character string specifying the directory path where the underlying bathymetric files are located.
- **previous_spatial_file**: A character string specifying the path to the previous spatial file. This may be used to build upon or modify existing spatial data.
- **rootdir_data**: A character string specifying the root directory path where project-related data is stored.
- **output_directory**: A character string specifying the directory where output files should be saved.
- **site_polygons_rdata**: A character string specifying the file name for the RData file containing the site polygons.
- **reefs_path**: A character string specifying the directory path where the benthic habitat raster file is located.
- **reef_file**: A character string specifying the name of the benthic habitat raster file (.tif).
- **gbr_zones_csv**: A character string specifying the path to the CSV file containing GBR zones information.
- **GBR_IDs**: A vector of IDs representing the Great Barrier Reef sites to be included in the analysis.
- **GBR_shape_file**: A character string specifying the path to the shapefile for the Great Barrier Reef.
- **site_size**: A numeric value specifying the desired size of the site polygons.
- **reshex**: A character string specifying the resolution of the hexagons to be used in the spatial analysis.
- **resolution**: An integer specifying the resolution level for the hexagons in the H3 grid system.
- **unit**: A character string specifying the unit of measurement for the hexagon area (e.g., "m²").
- **create_polygon**: A logical value indicating whether to create new site polygons. Defaults to `FALSE`.
- **add_site_to_polygon**: A logical value indicating whether to add spatial data to existing site polygons. Defaults to `TRUE`.


This function performs two main tasks:

1. **Create New Polygons**: If `create_polygon` is set to `TRUE`, the function calls the script to create new site polygons using the provided connectivity, bathymetric, and habitat data.
   
2. **Add Spatial Data to Existing Polygons**: If `add_site_to_polygon` is set to `TRUE`, the function adds spatial data to existing site polygons, such as bathymetric and habitat data, based on the connectivity and other provided information.

The function ensures that site polygons are generated or updated with relevant spatial data required for further analysis or modeling.

#### output

Returns an updated spatial object (typically an `sf` object) that includes the newly created or updated site polygons, enriched with spatial data like bathymetry, habitat composition, and connectivity information.

#### Example Usage

```r
Create_add_site_polygons(
  connectivity_folder = "path/to/connectivity",
  conn_files = "connectivity_file.csv",
  cluster_properties = cluster_properties_object,
  bathymetric_file = "bathymetry.tif",
  bathymetric_path = "path/to/bathymetry",
  ub_files = "ub_file.csv",
  ub_files_path = "path/to/ub_files",
  previous_spatial_file = "path/to/previous_spatial.RData",
  rootdir_data = "path/to/root_data",
  output_directory = "path/to/output",
  site_polygons_rdata = "site_polygons.RData",
  reefs_path = "path/to/reefs",
  reef_file = "benthic_habitat.tif",
  gbr_zones_csv = "GBR_zones.csv",
  GBR_IDs = c(1, 2, 3),
  GBR_shape_file = "path/to/GBR_shapefile.shp",
  site_size = 100,
  reshex = "resolution_hex",
  resolution = 9,
  unit = "m²",
  create_polygon = TRUE,
  add_site_to_polygon = TRUE
)

```



## Contributing

- Make sure to document any new functionality in the appropriate README file.
- Ensure code is well-commented and follows the project’s coding standards.
- For significant changes, consider creating a test case or validation script.

## License

This repository is licensed under MIT License.


# References
Great Barrier Reef non-cyclonic and on-reef wave model predictions
Callaghan, David (2023). Great Barrier Reef non-cyclonic and on-reef wave model predictions. The University of Queensland. 
Data Collection.https://doi.org/10.48610/8246441

Callaghan, D. P., Leon, J. X., & Saunders, M. I. (2015). Wave modelling as a proxy for seagrass ecological modelling: Comparing fetch and process-based predictions for a bay and reef lagoon. Estuarine, Coastal and Shelf Science, 153, 108–120. 

