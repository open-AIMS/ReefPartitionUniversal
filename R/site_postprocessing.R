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

  rows_to_remove <- c()
  extra_sites <- letters
  new_sites <- reef_site_polygons[1, ]
  site_polygons_crs <- sf::st_crs(reef_site_polygons)

  for (i in 1:nrow(reef_site_polygons)) {
    if (as.numeric(reef_site_polygons$area[i]) < min_site_area) {
      # Removes sites that are smaller than a minimum threshold
      rows_to_remove <- c(rows_to_remove, i)
      print(glue::glue("Polygon {i} too small"))
    } else {
      if (inherits(reef_site_polygons$geometry[i][1], "sfc_MULTIPOLYGON")) {
        # Processing Multi-polygons:
        # When a site consists of multiple polygons (sfc_MULTIPOLYGON)
        # Separates multi-polygons into individual polygons
        # Assigns new IDs using letters (a,b,c,d,e,f)
        # Creates new rows for each separated polygon
        new_polygons <- multipolygon_processing(
          polygon = reef_site_polygons[i, ],
          min_site_area,
          site_polygons_crs,
          max_distance = max_distance
        )

        if (is.null(new_polygons)) {
          rows_to_remove <- c(rows_to_remove, i)
          print(glue::glue("Polygon {i} contains 0 rows after filtering."))
          next
        }

        rows_to_remove <- c(rows_to_remove, i)

        new_rows <- reef_site_polygons[i, ] %>%
          dplyr::slice(rep(1:n(), each = nrow(new_polygons)))
        for (m in 1:nrow(new_polygons)) {
          new_rows$geometry[m] <- new_polygons$geometry[m]
        }

        new_rows$site_id <- paste0(
          new_rows$site_id,
          extra_sites[1:nrow(new_polygons)]
        )

        new_sites <- rbind(new_sites, new_rows)
      }
    }
  }

  if (length(rows_to_remove) > 0) {
    reef_site_polygons <- reef_site_polygons[-rows_to_remove, ]
    new_sites <- new_sites[-1, ]
    reef_site_polygons <- rbind(reef_site_polygons, new_sites)
  }

  return(reef_site_polygons)
}

multipolygon_processing <- function(
  polygon,
  min_site_area = 50 * 307,
  site_polygons_crs = sf::st_crs(polygon),
  max_distance = 100
) {
  separate_polygons <- data.frame(index = 1:length(polygon$geometry[[1]]))
  new_polygons <- data.frame(index = 1)
  poly_indices <- 1
  separate_polygons$area <- NA
  separate_polygons$geometry <- NA
  new_polygons$geometry <- NA
  separate_polygons$Number <- NA
  new_polygons$area <- NA

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
    separate_polygons$geometry[
      lists
    ] <- individual_poly
    separate_polygons$area[lists] <- sf::st_area(individual_poly)
  }

  separate_polygons <- sf::st_as_sf(separate_polygons, crs = site_polygons_crs)

  while (nrow(separate_polygons) > 0) {
    largest_poly <- which(
      separate_polygons$area == max(separate_polygons$area)
    )[1]
    distances <- sf::st_distance(separate_polygons)
    distances <- as.numeric(distances[largest_poly, ])

    if (any(distances[-largest_poly] < max_distance)) {
      # combine polygons into multipolygon
      Indices <- which(distances < max_distance)
      multi <- sf::st_union(sf::st_sfc(separate_polygons$geometry[Indices]))
      new_polygons[poly_indices, ] <- poly_indices
      new_polygons$geometry[poly_indices] <- multi
      new_polygons$area[poly_indices] <- sum(separate_polygons$area[Indices])
      separate_polygons <- separate_polygons[-Indices, ]
      poly_indices <- poly_indices + 1
    } else {
      new_polygons[poly_indices, ] <- poly_indices
      new_polygons$geometry[poly_indices] <- separate_polygons$geometry[
        largest_poly
      ]
      new_polygons$area[poly_indices] <- separate_polygons$area[largest_poly]
      separate_polygons <- separate_polygons[-largest_poly, ]
      poly_indices <- poly_indices + 1
    }
  }

  new_polygons <- new_polygons[which(new_polygons$area > min_site_area), ] # minmum polygon size to filter with

  if (nrow(new_polygons) == 0) {
    rlang::warn(
      "Multipolygon processing using min_site_area has resulted in 0 remaining polygons, please check area threshold.",
      class = "multipolygon_filtering"
    )

    return(NULL)
  }

  return(new_polygons)
}
