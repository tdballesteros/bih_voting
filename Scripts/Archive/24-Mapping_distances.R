

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# load distance data
mun_distances <- read.csv("Formatted Data/municipality_distances.csv")

# load formatted shapefiles
source("01-Format_shapefiles.R", echo = FALSE)

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp", quiet = TRUE)

### format data ------------------------------------------------------------------------------------
# death data df with geographic data
distance_postwar_municipality <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid() %>%
  dplyr::select(municipality = NAME_3, geometry) %>%
  dplyr::full_join(mun_distances, by = "municipality") %>%
  dplyr::mutate(
    # convert distances from m to km
    `Distance to Croatia, in km` = dist_hrv / 1000,
    `Distance to Serbia, in km` = dist_srb / 1000,
    `Distance to Montenegro, in km` = dist_mne / 1000,
    `Distance to Yugoslavia, in km` = dist_yug / 1000,
    `Distance to Another Country, in km` = dist_other_country / 1000,
    `Distance to the IEBL, in km` = dist_internal_border / 1000,
  )

### create maps ------------------------------------------------------------------------------------

#### Croatia ---------------------------------------------------------------------------------------
distance_to_hrv <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to Croatia, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#ff0000",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_hrv

#### Serbia ----------------------------------------------------------------------------------------
distance_to_srb <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to Serbia, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#0047ab",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_srb

#### Montenegro ------------------------------------------------------------------------------------
distance_to_mne <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to Montenegro, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#d4af37",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_mne

#### Yugoslavia ------------------------------------------------------------------------------------
distance_to_yug <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to Yugoslavia, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#800080",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_yug

#### Yugoslavia ------------------------------------------------------------------------------------
distance_to_other <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to Another Country, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#87ceeb",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_other

#### IEBL ------------------------------------------------------------------------------------------
distance_to_iebl <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = distance_postwar_municipality,
    ggplot2::aes(fill = `Distance to the IEBL, in km`)
  ) +
  ggplot2::scale_fill_gradient(
    low = "#c54a6f",
    high = "white",
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "darkblue", size = 0.5) +
  ggplot2::theme_void()

distance_to_iebl

### create graphs ----------------------------------------------------------------------------------
plot(distance_postwar_municipality$`Distance to Yugoslavia, in km`,
     distance_postwar_municipality$`Distance to Croatia, in km`)

