# ReefPartitionUniversal

<!-- badges: start -->
[![Documentation](https://img.shields.io/badge/documentation-blue)](https://open-aims.github.io/ReefPartitionUniversal/)
<!-- badges: end -->

# Package overview

The ReefPartitionAllenAtlas R package provides functions for partitioning
coral reef spatial areas into smaller sites (such as for logistic and monitoring
purposes) based on raster data layers. The package provides functions for extracting
pixel points on a reef based on habitat types, extracting values for additional
raster layers such as bathymetry. Pixels can then be clustered into sites within
habitat types based on their geographic attributes as well as additional variable
values such as pixel depth.

The package uses a flexible framework allowing multiple additional raster layers
to be used, along with user defined pixel clustering algorithms. The package itself
defines two spatial clustering algorithm options, using adespatial::constr.hclust
[(Guénard and Legendre, 2022)](https://www.jstatsoft.org/article/view/v103i07) and spdep::skater [(AssunÇão et al. 2007)](https://www.tandfonline.com/doi/full/10.1080/13658810600665111#d1e2053).

# Installation
```R
# Install package using GitHub repo
remotes::install_github("open-AIMS/ReefPartitionUniversal")
```

The package combines functionality from raster and vector data processing packages.
Dependencies include: sf, terra, sfnetworks, h3, igraph, spdep, adespatial, dplyr 
and magrittr

## Development installation
The most convenient way to install the package for local development is to clone
the GitHub repository, create a branch and use the following command to install
the package from source code.
```R
# Install package from local source folder/repo
devtools::install("path to package folder")
```

# Basic demonstration

The following code demonstrates a basic usage of ReefPartitionUniversal using
`adespatial::constr.hclust` clustering algorithm and Minimum Spanning Tree inputs.
For more

```R
library(ReefPartitionUniversal)

# Load input data (ensuring all are use the same CRS)
target_reef <- sf::st_read("target_reef.gpkg") # Defines the spatial extent of the reef
habitat <- terra::rast("habitat_raster.tif") # Defines the habitat pixels to extract from
bathymetry <- terra::rast("bathymetry_raster.tif") # Contains additional variable values for extraction and clustering

habitat_categories <- c(1, 10, 20) # Assess only habitat pixels with these values

# Extract pixel values from raster layers
pixels <- extract_pixel_points(target_reef, habitat, bathymetry, habitat_categories)
pixels <- pixels[!is.na(pixels$depth), ]
pixels$UNIQUE_ID <- "ReefOne"

# Cluster pixels using adespatial::constr.hclust algorithm
# The
mst_hclust_pixels <- cluster_reef_pixels(pixels)

# Collate pixels from each site/cluster into polygons
mst_hclust_sites <- clustered_pixels_to_polygons(mst_hclust_pixels)

# Optional: Apply post-processing to pixel clusters to ensure that non-contiguous
# clusters adhere to a maximum distance between areas.
sites_post_processed <- site_postprocessing(mst_hclust_sites, 50)
```

# License

This repository is licensed under MIT License.

# Development

Any problems and/or suggestions encountered with this package can be logged in
as GitHub issues.

This R package follows the [tidyverse styleguide](https://style.tidyverse.org/).

## Formatting

Code in this package can be auto-formatted to follow the tidyverse styleguide using
a formatter such as [Air](https://posit-dev.github.io/air/). Once installed for
the chosen IDE, using Air commands will reformat R files, modifying the whitespace,
linespace and punctuation to follow the tidyverse styleguide.

Additionally, Air has been set up to check file formatting when a new Pull Request
is made.

