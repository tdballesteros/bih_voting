
# This script maps the proportion of municipalities identifying as each ethnicity in the 1991
# census.

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# 1991 census data
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# load formatted shapefiles
source("01-Format_shapefiles.R", echo = FALSE)

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp", quiet = TRUE)

### format data ------------------------------------------------------------------------------------
# population df with geographic data
population_df <- bih_prewar_municipalities_shapefile %>%
  sf::st_make_valid() %>%
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  dplyr::select(municipality, percent_muslims, percent_croats, percent_serbs, percent_yugoslavs,
                percent_other, geometry) %>%
  dplyr::mutate(across(all_of(c("percent_muslims", "percent_croats", "percent_serbs",
                                "percent_yugoslavs", "percent_other")), ~ .x * 100)) %>%
  dplyr::rename(
    `Percent\nMuslims` = percent_muslims,
    `Percent\nCroats` = percent_croats,
    `Percent\nSerbs` = percent_serbs,
    `Percent\nYugoslavs` = percent_yugoslavs,
    `Percent\nOther` = percent_other
  )

### create maps ------------------------------------------------------------------------------------
prewar_muslims <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = population_df,
    ggplot2::aes(fill = `Percent\nMuslims`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "#008000", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::theme_void()

prewar_muslims

prewar_croats <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = population_df,
    ggplot2::aes(fill = `Percent\nCroats`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "#ff0000", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::theme_void()

prewar_croats

prewar_serbs <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = population_df,
    ggplot2::aes(fill = `Percent\nSerbs`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "#0047ab", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::theme_void()

prewar_serbs

prewar_yugoslav <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = population_df,
    ggplot2::aes(fill = `Percent\nYugoslavs`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "#800080", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
ggplot2::theme_void()

prewar_yugoslav

prewar_other <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = population_df,
    ggplot2::aes(fill = `Percent\nOther`)
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "yellow", na.value = "grey70") +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.5) +
  ggplot2::theme_void()

prewar_other
