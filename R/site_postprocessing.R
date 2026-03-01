#' Perform post-processing steps on site polygons.
#'
#' @description Perform post-processing steps on site polygons for a reef. Post-
#'   processing applies to multipolygons (sites made up of smaller non-continuous
#'   polygons) only. This process involves identifying the largest single polygon and
#'   removing any polygons that are outside a user-defined distance threshold from the largest
#'   polygon. Post-processing also involves removing any site polygons that are too
#'   small, based on a user defined minimum site area.
#'
#' @param reef_site_polygons data.frame. Contains a row for each unique site, for
#'   a target reef of interest.
#' @param min_site_area numeric. Minimum threshold for removing sites that are too
#'   small in their total site area. This value should be smaller than the site_area
#'   used in other parts of the workflow as it is intended as an absolute minimum
#'   threshold. Must be in the same units returned by `sf::st_area()`.
#'   Default value = 50 * 307, where 307 is the area (in m^2) of a H3 cell with
#'   resolution of 12 and 50 is the minimum number of hexagons per site.
#'
#' @return data.frame containing all site polygons for the target reef after post-
#'   processing has taken place on undersized or multipolygon sites.
#'
#' @importFrom dplyr n
#'
#' @export
#'
site_postprocessing <- function(
  reef_site_polygons,
  min_site_area = 50 * 307,
  max_distance = 100
) {
  reef_site_polygons$area <- sf::st_area(reef_site_polygons)

  RowsToRemove <- c()
  ExtraSites <- c("a", "b", "c", "d", "e", "f")
  NewSites <- reef_site_polygons[1, ]
  site_polygons_crs <- sf::st_crs(reef_site_polygons)

  for (i in 1:nrow(reef_site_polygons)) {
    # print(i)
    if (as.numeric(reef_site_polygons$area[i]) < min_site_area) {
      # Removes sites that are smaller than a minimum threshold
      RowsToRemove <- c(RowsToRemove, i)
      print(glue::glue("Polygon {i} too small"))
    } else {
      if (class(reef_site_polygons$geometry[i])[1] == "sfc_MULTIPOLYGON") {
        # Processing Multi-polygons:
        # When a site consists of multiple polygons (sfc_MULTIPOLYGON)
        # Separates multi-polygons into individual polygons
        # Assigns new IDs using letters (a,b,c,d,e,f)
        # Creates new rows for each separated polygon
        NewPolygons <- multipolygon_processing(
          polygon = reef_site_polygons[i, ],
          min_site_area,
          site_polygons_crs,
          max_distance = max_distance
        )

        if (is.null(NewPolygons)) {
          RowsToRemove <- c(RowsToRemove, i)
          print(glue::glue("Polygon {i} contains 0 rows after filtering."))
          next
        }

        RowsToRemove <- c(RowsToRemove, i)

        NewRows <- reef_site_polygons[i, ] %>%
          dplyr::slice(rep(1:n(), each = nrow(NewPolygons)))
        for (m in 1:nrow(NewPolygons)) {
          NewRows$geometry[m] <- NewPolygons$geometry[m]
        }

        NewRows$site_id <- paste0(
          NewRows$site_id,
          ExtraSites[1:nrow(NewPolygons)]
        )

        NewSites <- rbind(NewSites, NewRows)
      }
    }
  }

  reef_site_polygons <- reef_site_polygons[-RowsToRemove, ]
  NewSites <- NewSites[-1, ]
  reef_site_polygons <- rbind(reef_site_polygons, NewSites)
}

multipolygon_processing <- function(
  polygon,
  min_site_area = 50 * 307,
  site_polygons_crs = 4326,
  max_distance = 100
) {
  PolygonSeperate <- data.frame(index = 1:length(polygon$geometry[[1]]))
  NewPolygons <- data.frame(index = 1)
  NumberPolygons <- 1
  PolygonSeperate$area <- NA
  PolygonSeperate$geometry <- NA
  NewPolygons$geometry <- NA
  PolygonSeperate$Number <- NA
  NewPolygons$area <- NA

  # Separate the polygons that are contained in the target multipolygon feature
  # into individual polygon elements in a data frame.
  for (lists in 1:length(polygon$geometry[[1]])) {
    individual_poly <- sf::st_sfc(
      sf::st_polygon(polygon$geometry[[1]][[
        lists
      ]]),
      crs = site_polygons_crs
    ) %>%
      sf::st_set_crs(site_polygons_crs)
    PolygonSeperate$geometry[
      lists
    ] <- individual_poly
    PolygonSeperate$area[lists] <- sf::st_area(individual_poly)
  }

  PolygonSeperate <- sf::st_as_sf(PolygonSeperate, crs = site_polygons_crs)

  while (nrow(PolygonSeperate) > 0) {
    LargestIndex <- which(PolygonSeperate$area == max(PolygonSeperate$area))[1]
    Dist <- sf::st_distance(PolygonSeperate)
    Dist <- as.numeric(Dist[LargestIndex, ])

    if (any(Dist[-LargestIndex] < max_distance)) {
      # combine polygons into multipolygon
      Indices <- which(Dist < max_distance)
      Multi <- sf::st_union(sf::st_sfc(PolygonSeperate$geometry[Indices]))
      NewPolygons[NumberPolygons, ] <- NumberPolygons
      NewPolygons$geometry[NumberPolygons] <- Multi
      NewPolygons$area[NumberPolygons] <- sum(PolygonSeperate$area[Indices])
      PolygonSeperate <- PolygonSeperate[-Indices, ]
      NumberPolygons <- NumberPolygons + 1
    } else {
      NewPolygons[NumberPolygons, ] <- NumberPolygons
      NewPolygons$geometry[NumberPolygons] <- PolygonSeperate$geometry[
        LargestIndex
      ]
      NewPolygons$area[NumberPolygons] <- PolygonSeperate$area[LargestIndex]
      PolygonSeperate <- PolygonSeperate[-LargestIndex, ]
      NumberPolygons <- NumberPolygons + 1
    }
  }

  NewPolygons <- NewPolygons[which(NewPolygons$area > min_site_area), ] # minmum polygon size to filter with

  if (nrow(NewPolygons) == 0) {
    rlang::warn(
      "Multipolygon processing using min_site_area has resulted in 0 remaining polygons, please check area threshold.",
      class = "multipolygon_filtering"
    )

    return(NULL)
  }

  return(NewPolygons)
}
