# Partitioning a generic reef

``` r

remotes::install_github("open-aims/ReefPartitionUniversal", ref="pkgdown-site")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo open-aims/ReefPartitionUniversal@pkgdown-site
#> magrittr (2.0.3     -> 2.0.4    ) [CRAN]
#> jsonlite (1.8.9     -> 2.0.0    ) [CRAN]
#> purrr    (1.0.2     -> 1.2.1    ) [CRAN]
#> XML      (3.99-0.20 -> 3.99-0.22) [CRAN]
#> tibble   (3.2.1     -> 3.3.1    ) [CRAN]
#> Installing 5 packages: magrittr, jsonlite, purrr, XML, tibble
#> Installing packages into 'C:/Users/grier/AppData/Local/Temp/Rtmpwb622X/temp_libpath1370c76a45617'
#> (as 'lib' is unspecified)
#> 
#>   There is a binary version available but the source version is later:
#>        binary    source needs_compilation
#> XML 3.99-0.20 3.99-0.22              TRUE
#> 
#> package 'magrittr' successfully unpacked and MD5 sums checked
#> package 'jsonlite' successfully unpacked and MD5 sums checked
#> package 'purrr' successfully unpacked and MD5 sums checked
#> package 'tibble' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\grier\AppData\Local\Temp\RtmpIF4p2G\downloaded_packages
#> installing the source package 'XML'
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file 'C:\Users\grier\AppData\Local\Temp\RtmpIF4p2G\remotes142c2c941ef3\open-AIMS-ReefPartitionUniversal-82b055f/DESCRIPTION' ... OK
#> * preparing 'ReefPartitionUniversal':
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building 'ReefPartitionUniversal_0.0.0.9000.tar.gz'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/articles/workflow_files/figure-html/generating example bathymetry data-1.png'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/articles/workflow_files/figure-html/generating example habitat data-1.png'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3-UBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3CUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3GUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3KUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3OUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMa3iUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMawCUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/KFO7CnqEu92Fr1ME7kSn66aGLdTylUAMaxKUBGEe.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqW106F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWt06F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWtE6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWtU6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWtk6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWu06F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWuU6F.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWuk6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWvU6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memtYaGs126MiZpBA-UFUIcVXSCEkx2cmqvXlWqWxU6F15M.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTS-muw.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTS2mu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSCmu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSGmu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSKmu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSOmu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSumu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSymu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTUGmu1aB.woff2'
#> Warning in utils::tar(filepath, pkgname, compression = compression, compression_level = 9L,  :
#>   storing paths of more than 100 bytes is not portable:
#>   'ReefPartitionUniversal/docs/deps/bootstrap-5.3.1/fonts/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTVOmu1aB.woff2'
#> 
#> Installing package into 'C:/Users/grier/AppData/Local/Temp/Rtmpwb622X/temp_libpath1370c76a45617'
#> (as 'lib' is unspecified)
library(ReefPartitionUniversal)
```

## Generating example raster data

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

plot(bathy)
```

![](workflow_files/figure-html/generating%20example%20bathymetry%20data-1.png)
