# Partitioning a generic reef

``` r

remotes::install_github("open-aims/ReefPartitionUniversal", ref="pkgdown-site")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo open-aims/ReefPartitionUniversal@pkgdown-site
#> magrittr (2.0.3 -> 2.0.4) [CRAN]
#> jsonlite (1.8.9 -> 2.0.0) [CRAN]
#> purrr    (1.0.2 -> 1.2.1) [CRAN]
#> tibble   (3.2.1 -> 3.3.1) [CRAN]
#> Installing 4 packages: magrittr, jsonlite, purrr, tibble
#> Installing packages into 'C:/Users/grier/AppData/Local/Temp/RtmpY51Z7m/temp_libpath126ec5c1a3a5'
#> (as 'lib' is unspecified)
#> package 'magrittr' successfully unpacked and MD5 sums checked
#> package 'jsonlite' successfully unpacked and MD5 sums checked
#> package 'purrr' successfully unpacked and MD5 sums checked
#> package 'tibble' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\grier\AppData\Local\Temp\Rtmp4wC75X\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file 'C:\Users\grier\AppData\Local\Temp\Rtmp4wC75X\remotes12054bbf5bfc\open-AIMS-ReefPartitionUniversal-8021978/DESCRIPTION' ... OK
#> * preparing 'ReefPartitionUniversal':
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building 'ReefPartitionUniversal_0.0.0.9000.tar.gz'
#> 
#> Installing package into 'C:/Users/grier/AppData/Local/Temp/RtmpY51Z7m/temp_libpath126ec5c1a3a5'
#> (as 'lib' is unspecified)
library(ReefPartitionUniversal)
```

First, we generate an example raster extent using the `terra` package
and an example reef polygon using the `sf` package.

``` r

library(terra)
#> Warning: package 'terra' was built under R version 4.4.3
#> terra 1.8.93
library(sf)
#> Warning: package 'sf' was built under R version 4.4.3
#> Linking to GEOS 3.13.0, GDAL 3.10.1, PROJ 9.5.1; sf_use_s2() is TRUE

# Set seed for reproducibility
set.seed(123)

# Create raster template
r <- rast(ncol=50, nrow=50, xmin=0, xmax=50, ymin=0, ymax=50)
reef_polygon <- st_as_sfc(st_bbox(r))
```

Next, we generate example habitat data using the raster extent. Habitat
NA values indicate no reef habitat of interest and values of 1 and 2
represent reef habitat types 1 and 2 (e.g. inshore/offshore or reef
flat/reef slope).

``` r

habitat <- r
values(habitat) <- sample(c(NA, 1, 2), ncell(habitat), replace = TRUE)
plot(habitat)
```

![](workflow_files/figure-html/generating%20example%20habitat%20data-1.png)

Finally, we generate example bathymetry data. In this example we use the
distance of each cell from the centre as an “example” of bathymetry,
adding random variation.

``` r

# Reef bathymetry layer
xc <- (xmin(r) + xmax(r)) / 2
yc <- (ymin(r) + ymax(r)) / 2

# Compute distance to centre of raster for each cell
coords <- crds(r)  # n x 2 matrix of xy coordinates
dist_center <- sqrt((coords[,1] - xc)^2 + (coords[,2] - yc)^2)

# Assign to raster
bathy <- r
values(bathy) <- dist_center + runif(length(dist_center), -2, 2)

# Invert so center = 0, edges = higher
bathy <- max(values(bathy)) - bathy

plot(bathy)
```

![](workflow_files/figure-html/generating%20example%20bathymetry%20data-1.png)
