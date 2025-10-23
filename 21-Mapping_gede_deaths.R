

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# ucdp gede data
ucdp_deaths_prewar_municipality <- read.csv("Formatted Data/ucdp_deaths_prewar_municipality_formatted.csv")
ucdp_deaths_postwar_municipality <- read.csv("Formatted Data/ucdp_deaths_postwar_municipality_formatted.csv")

# city coordinates
city_coords <- readxl::read_xlsx("Data/city_coordinates.xlsx")

# load formatted shapefiles
source("01-Format_shapefiles.R")

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp")

# gede dataset
source("~/R/bih_voting/03-UCDP_death_data_formatting.R")

### format data ------------------------------------------------------------------------------------
# convert city coordinates data to sf object
city_coords <- city_coords %>%
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

# # calculate municipality centroids
# centroids <- bih_postwar_municipalities_shapefile_formatted %>%
#   sf::st_make_valid() %>%
#   dplyr::group_by(NAME_3) %>%
#   dplyr::summarise(
#     centroid = sf::st_centroid(geometry)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(municipality = NAME_3, centroid)

# death data df with geographic data
ucdp_deaths_prewar_municipality <- bih_prewar_municipalities_shapefile %>%
  sf::st_make_valid() %>%
  dplyr::select(municipality, geometry) %>%
  dplyr::full_join(ucdp_deaths_prewar_municipality, by = "municipality") %>%
  dplyr::mutate(
    deaths_per_100000_logged = dplyr::case_when(
      best == 0 ~ 0,
      best > 0 ~ log(deaths_per_100000),
      best < 0 ~ log(deaths_per_100000) + 1
    ),
    best_logged = dplyr::case_when(
      best == 0 ~ 0,
      best > 0 ~ log(best),
      best < 0 ~ log(best) + 1
    ))

ucdp_deaths_postwar_municipality <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid() %>%
  dplyr::select(municipality = NAME_3, canton = NAME_2, entity = NAME_1, geometry) %>%
  dplyr::full_join(ucdp_deaths_postwar_municipality, by = "municipality") %>%
  mutate(across(c(count_of_events, best, high, low), ~replace_na(.x, 0))) %>%
  dplyr::mutate(
    best_logged = dplyr::case_when(
      best == 0 ~ 0,
      best > 0 ~ log(best),
      best < 0 ~ log(best) + 1
    ))

### Pre-War Municipalities -------------------------------------------------------------------------

# per capita
deaths_prewar_municipalities_top5_pc <- ucdp_deaths_prewar_municipality %>%
  dplyr::slice_max(deaths_per_100000, n = 5) %>%
  dplyr::pull(municipality)

deaths_prewar_municipalities_top5_pc_shapefile <- ucdp_deaths_prewar_municipality %>%
  dplyr::filter(municipality %in% deaths_prewar_municipalities_top5) %>%
  dplyr::mutate(label = paste0(municipality,": ",best," deaths"))

# legend breaks
original_breaks <- c(0, 40, 400, 4000, 40000) 

logged_break_positions <- case_when(
  original_breaks > 0 ~ log(original_breaks),
  original_breaks == 0 ~ 0,
  .default = NA
  )

final_labels <- scales::label_number(big.mark = ",")(original_breaks[original_breaks >= 0])

ucdp_deaths_per_capita_prewar_municipalities_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ucdp_deaths_prewar_municipality,
    ggplot2::aes(fill = deaths_per_100000_logged)
  ) +
  ggplot2::scale_fill_gradient(
    name = "Deaths per\n100,000",
    breaks = logged_break_positions,
    labels = final_labels,
    low = "white",
    high = "darkred",
    na.value = "grey70"
    ) +
  # ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::geom_sf(data = deaths_prewar_municipalities_top5_pc_shapefile, color = "black", size = 0.75) +
  ggplot2::geom_sf_label(data = deaths_prewar_municipalities_top5_pc_shapefile,
                         ggplot2::aes(label = municipality),
                         color = "black",
                         fill = "white",
                         label.padding = unit(0.15, "lines"),
                         show.legend = "point",
                         size = 2.5) +
  ggplot2::theme_void()

ucdp_deaths_per_capita_prewar_municipalities_map

# absolute
deaths_prewar_municipalities_top5_abs <- ucdp_deaths_prewar_municipality %>%
  dplyr::slice_max(best, n = 5) %>%
  dplyr::pull(municipality)

deaths_prewar_municipalities_top5_abs_shapefile <- ucdp_deaths_prewar_municipality %>%
  dplyr::filter(municipality %in% deaths_prewar_municipalities_top5_abs) %>%
  dplyr::mutate(label = paste0(municipality,": ",best," deaths"))

# legend breaks
original_breaks <- c(0, 15, 150, 1500, 15000) 

logged_break_positions <- case_when(
  original_breaks > 0 ~ log(original_breaks),
  original_breaks == 0 ~ 0,
  .default = NA
)

final_labels <- scales::label_number(big.mark = ",")(original_breaks[original_breaks >= 0])

ucdp_deaths_absolute_prewar_municipalities_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ucdp_deaths_prewar_municipality,
    ggplot2::aes(fill = best_logged)
  ) +
  ggplot2::scale_fill_gradient(
    name = "Deaths",
    breaks = logged_break_positions,
    labels = final_labels,
    low = "white",
    high = "darkred",
    na.value = "grey70"
  ) +
  # ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::geom_sf(data = deaths_prewar_municipalities_top5_abs_shapefile, color = "black", size = 0.75) +
  ggplot2::geom_sf_label(data = deaths_prewar_municipalities_top5_abs_shapefile,
                         ggplot2::aes(label = municipality),
                         color = "black",
                         fill = "white",
                         label.padding = unit(0.15, "lines"),
                         show.legend = "point",
                         size = 2.5) +
  ggplot2::theme_void()

ucdp_deaths_absolute_prewar_municipalities_map

# per capita + event points
gede_bih_geocoded_formatted <- 

ucdp_deaths_per_capita_prewar_municipalities_points_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ucdp_deaths_prewar_municipality,
    ggplot2::aes(fill = deaths_per_100000_logged)
  ) +
  ggplot2::scale_fill_gradient(
    name = "Deaths per\n100,000",
    breaks = logged_break_positions,
    labels = final_labels,
    low = "white",
    high = "darkred",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(
    data = gede_bih_geocoded,
    ggplot2::aes(size = log(best + 1))) +
  ggplot2::theme_void()

ucdp_deaths_per_capita_prewar_municipalities_points_map

### Post-War Municipalities ------------------------------------------------------------------------

# absolute
deaths_postwar_municipalities_top5_abs <- ucdp_deaths_postwar_municipality %>%
  dplyr::slice_max(best, n = 5) %>%
  dplyr::pull(municipality)

deaths_postwar_municipalities_top5_abs_shapefile <- ucdp_deaths_postwar_municipality %>%
  dplyr::filter(municipality %in% deaths_postwar_municipalities_top5_abs) %>%
  dplyr::mutate(label = paste0(municipality,": ",best," deaths"))

# legend breaks
original_breaks <- c(0, 15, 150, 1500, 15000) 

logged_break_positions <- case_when(
  original_breaks > 0 ~ log(original_breaks),
  original_breaks == 0 ~ 0,
  .default = NA
)

final_labels <- scales::label_number(big.mark = ",")(original_breaks[original_breaks >= 0])

ucdp_deaths_absolute_postwar_municipalities_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ucdp_deaths_postwar_municipality,
    ggplot2::aes(fill = best_logged)
  ) +
  ggplot2::scale_fill_gradient(
    name = "Deaths",
    breaks = logged_break_positions,
    labels = final_labels,
    low = "white",
    high = "darkred",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = deaths_postwar_municipalities_top5_abs_shapefile, color = "black", size = 0.75) +
  ggplot2::geom_sf_label(data = deaths_postwar_municipalities_top5_abs_shapefile,
                         ggplot2::aes(label = municipality),
                         color = "black",
                         fill = "white",
                         label.padding = unit(0.15, "lines"),
                         show.legend = "point",
                         size = 2.5) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

ucdp_deaths_absolute_postwar_municipalities_map
