
### load libraries ---------------------------------------------------------------------------------
library(sf)
library(readxl)
library(mapview)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# UCDP dataset
gede <- readxl::read_xlsx("/Users/timmyballesteros/Downloads/GEDEvent_v25_1.xlsx")

# formatted 1991 census data
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# load formatted shapefiles
source("01-Format_shapefiles.R")

### format data ------------------------------------------------------------------------------------
gede_bih <- gede %>%
  dplyr::filter(
    country == "Bosnia-Herzegovina"
    ) %>%
  # drop unneeded columns
  dplyr::select(-c(active_year, code_status, number_of_sources, source_article, source_office,
                   source_date, source_headline, source_original, adm_1, adm_2, geom_wkt,
                   priogrid_gid, country, country_id, region, gwnoa, gwnob))

gede_bih_geocoded <- gede_bih %>%
  dplyr::filter(
    # filter out location precision of 6 (country-level))
    where_prec %in% c(1, 2, 5)
    ) %>%
  # convert to an sf object
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
    ) %>%
  dplyr::mutate(
    # calculate the length of each event and add one so date range is inclusive of endpoints
    event_length = difftime(date_end, date_start, units = "days") + 1
    )

### calculate death summary statistics -------------------------------------------------------------
# total deaths
sum(gede_bih$best, na.rm = FALSE)

# geocoded deaths
sum(gede_bih_geocoded$best, na.rm = FALSE)

# UCDP's count is ~2/3 of Costalli & Moro's count.

### prewar municipality mapping --------------------------------------------------------------------
# confirm CRSs match
sf::st_crs(gede2)$proj4string
sf::st_crs(bih_prewar_municipalities_shapefile)$proj4string

# spatial join
gede_municipality_prewar_assignment <- sf::st_join(
  gede2,
  bih_prewar_municipalities_shapefile,
  join = st_within,
  left = TRUE
)

# collapse by municipality
gede_municipality_prewar_counts <- gede_municipality_prewar_assignment %>%
  dplyr::group_by(municipality) %>%
  dplyr::summarise(
    count_of_events = n(),
    best = sum(best, na.rm = TRUE),
    high = sum(high, na.rm = TRUE),
    low = sum(low, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(municipality) %>%
  # add 1991 census data
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  mutate(across(c(count_of_events, best, high, low), ~replace_na(.x, 0))) %>%
  dplyr::mutate(
    deaths_per_100000 = 100000 * best / total,
    deaths_per_100000_high = 100000 * high / total,
    deaths_per_100000_low = 100000 * low / total
  ) %>%
  as.data.frame() %>%
  dplyr::select(municipality, count_of_events, best, high, low, deaths_per_100000,
                deaths_per_100000_high, deaths_per_100000_low)

# write formatted data
write.csv(gede_municipality_prewar_counts,
          "Formatted Data/ucdp_deaths_prewar_municipality_formatted.csv",
          row.names = FALSE)

### postwar municipality mapping -------------------------------------------------------------------
bih_postwar_municipalities_shapefile_formatted <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid()

# confirm CRSs match
sf::st_crs(gede2)$proj4string
sf::st_crs(bih_postwar_municipalities_shapefile_formatted)$proj4string

# spatial join
gede_municipality_postwar_assignment <- sf::st_join(
  gede2,
  bih_postwar_municipalities_shapefile_formatted,
  join = st_within,
  left = TRUE
)

# collapse by municipality
gede_municipality_postwar_counts <- gede_municipality_postwar_assignment %>%
  dplyr::group_by(NAME_3) %>%
  dplyr::summarise(
    count_of_events = n(),
    best = sum(best, na.rm = TRUE),
    high = sum(high, na.rm = TRUE),
    low = sum(low, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(municipality = NAME_3) %>%
  dplyr::arrange(municipality) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

# write formatted data
write.csv(gede_municipality_postwar_counts,
          "Formatted Data/ucdp_deaths_postwar_municipality_formatted.csv",
          row.names = FALSE)

