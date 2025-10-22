

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# nationalist / non-nationalist proportion of votes by municipality in the 1997 elections
nat_votes_1997 <- read.csv("Formatted Data/nationalist_votes_1997.csv")

# city coordinates
city_coords <- readxl::read_xlsx("Data/city_coordinates.xlsx")

# load formatted shapefiles
source("0X-Format_shapefiles.R")

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp")

### format data ------------------------------------------------------------------------------------
# convert city coordinates data to sf object
city_coords <- city_coords %>%
  sf::st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

# calculate municipality centroids
centroids <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid() %>%
  dplyr::group_by(NAME_3) %>%
  dplyr::summarise(
    centroid = sf::st_centroid(geometry)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(municipality = NAME_3, centroid)

# vote-share df with geographic data
nationalist_df <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid() %>%
  dplyr::select(municipality = NAME_3, canton = NAME_2, entity = NAME_1, geometry) %>%
  dplyr::full_join(nat_votes_1997, by = "municipality") %>%
  dplyr::mutate(across(all_of(c("Nationalist", "Non.Nationalist", "Other", "Invalid.Ballot")), ~ .x * 100)) %>%
  dplyr::rename(
    `Nationalist Party\nVoteshare` = Nationalist,
    `Non-Nationalist\nParty Voteshare` = Non.Nationalist
    )

### Election Results 1997: Nationalist % of Vote ---------------------------------------------------
nationalist_top5 <- nationalist_df %>%
  dplyr::slice_max(`Nationalist Party\nVoteshare`, n = 5) %>%
  dplyr::pull(municipality)

nationalist_top5_shapefile <- centroids %>%
  dplyr::filter(municipality %in% nationalist_top5)

nationalist_bottom5 <- nationalist_df %>%
  dplyr::slice_min(`Nationalist Party\nVoteshare`, n = 5) %>%
  dplyr::pull(municipality)

nationalist_bottom5_shapefile <- centroids %>%
  dplyr::filter(municipality %in% nationalist_bottom5)

nationalist_proportion_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = nationalist_df,
    ggplot2::aes(fill = `Nationalist Party\nVoteshare`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkred", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::geom_sf(data = nationalist_top5_shapefile, color = "black", size = 0.75) +
  # ggplot2::geom_sf(data = nationalist_bottom5_shapefile, color = "black", size = 0.75) +
  ggplot2::geom_sf_label(data = nationalist_top5_shapefile,
                         ggplot2::aes(label = municipality),
                         color = "black",
                         fill = "white",
                         label.padding = unit(0.15, "lines"),
                         nudge_x = 0.1,
                         nudge_y = 0.05,
                         show.legend = "point",
                         size = 2.5) +
  # ggplot2::geom_sf_label(data = nationalist_bottom5_shapefile,
  #                        ggplot2::aes(label = municipality),
  #                        color = "white",
  #                        fill = "black",
  #                        label.padding = unit(0.15, "lines"),
  #                        nudge_x = 0.14,
  #                        nudge_y = 0.05,
  #                        show.legend = "point",
  #                        size = 2.5) +
  # ggplot2::geom_sf(data = city_coords, color = "black", size = .75) +
  # ggplot2::geom_sf_label(data = city_coords,
  #              ggplot2::aes(label = City),
  #              color = "black",
  #              fill = "white",
  #              label.padding = unit(0.15, "lines"),
  #              nudge_x = 0.14,
  #              nudge_y = 0.05,
  #              show.legend = "point",
  #              size = 2.5) +
  ggplot2::theme_void()

nationalist_proportion_map

### Election Results 1997: Non-Nationalist % of Vote -----------------------------------------------
nonnationalist_top5 <- nationalist_df %>%
  dplyr::slice_max(`Non-Nationalist\nParty Voteshare`, n = 5) %>%
  dplyr::pull(municipality)

nonnationalist_top5_shapefile <- centroids %>%
  dplyr::filter(municipality %in% nonnationalist_top5)

nonnationalist_bottom5 <- nationalist_df %>%
  dplyr::slice_min(`Non-Nationalist\nParty Voteshare`, n = 5) %>%
  dplyr::pull(municipality)

nonnationalist_bottom5_shapefile <- centroids %>%
  dplyr::filter(municipality %in% nonnationalist_bottom5)

nonnationalist_proportion_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = nationalist_df,
    ggplot2::aes(fill = `Non-Nationalist\nParty Voteshare`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkred", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::geom_sf(data = nonnationalist_top5_shapefile, color = "black", size = 0.75) +
  # ggplot2::geom_sf(data = nonnationalist_bottom5_shapefile, color = "black", size = 0.75) +
  ggplot2::geom_sf_label(data = nonnationalist_top5_shapefile,
                         ggplot2::aes(label = municipality),
                         color = "black",
                         fill = "white",
                         label.padding = unit(0.15, "lines"),
                         nudge_x = 0.1,
                         nudge_y = 0.05,
                         show.legend = "point",
                         size = 2.5) +
  # ggplot2::geom_sf_label(data = nonnationalist_bottom5_shapefile,
  #                        ggplot2::aes(label = municipality),
  #                        color = "white",
  #                        fill = "black",
  #                        label.padding = unit(0.15, "lines"),
  #                        nudge_x = 0.14,
  #                        nudge_y = 0.05,
  #                        show.legend = "point",
  #                        size = 2.5) +
  # ggplot2::geom_sf(data = city_coords, color = "black", size = .75) +
  # ggplot2::geom_sf_label(data = city_coords,
  #              ggplot2::aes(label = City),
  #              color = "black",
  #              fill = "white",
  #              label.padding = unit(0.15, "lines"),
  #              nudge_x = 0.14,
  #              nudge_y = 0.05,
  #              show.legend = "point",
  #              size = 2.5) +
  ggplot2::theme_void()

nonnationalist_proportion_map
