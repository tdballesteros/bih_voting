

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(geodata)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# load formatted shapefiles
source("~/R/bih_voting/0X-Format_shapefiles.R", echo = FALSE)

# country outline
bih_outline_shapefile <- sf::st_read("Shape Files/boundary/BIH_adm0.shp")

# post-war entity shapefile
bih_postwar_entities_shapefile <- sf::st_read("Shape Files/admin1/BIH_adm1.shp")

# post-war municipality shapefile
# bih_postwar_municipalities_shapefile_formatted <- sf::st_read("Shape Files/admin3/BIH_adm3.shp")

# load shapefiles for countries surrounding Bosnia and Herzegovina
hrv_shapefile <- geodata::gadm(country = "HRV", level = 0)
srb_shapefile <- geodata::gadm(country = "SRB", level = 0)
mne_shapefile <- geodata::gadm(country = "MNE", level = 0)
xko_shapefile <- geodata::gadm(country = "XKO", level = 0)

### format data ------------------------------------------------------------------------------------
# create shapefile of united Serbia and Montenegro (rump Yugoslavia)
yug_shapefile <- srb_shapefile %>%
  rbind(mne_shapefile) %>%
  rbind(xko_shapefile) %>%
  sf::st_as_sf() %>%
  sf::st_union() %>%
  sf::st_as_sf()

# create shapefile of internal first-level post-war administrative boundaries (internal boundaries
# between Republika Srpska, the Federation, and Brčko District)
sf::sf_use_s2(FALSE) 
country_outline_line <- sf::st_boundary(sf::st_union(bih_postwar_entities_shapefile))
all_boundaries <- sf::st_boundary(bih_postwar_entities_shapefile)
internal_divisions_sfc <- sf::st_difference(all_boundaries, country_outline_line)
internal_lines <- internal_divisions_sfc %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_union() %>%
  sf::st_sf() 
sf::sf_use_s2(TRUE)

# write data
sf::write_sf(internal_lines, "Shape Files/internal_entity_division_shapefile.shp")

# calculate distances from each municipality to HRV, SRB, MNE and rump YUG
mun_distances <- bih_postwar_municipalities_shapefile_formatted %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    dist_hrv = sf::st_distance(geometry, sf::st_as_sf(hrv_shapefile)),
    dist_srb = sf::st_distance(geometry, sf::st_as_sf(srb_shapefile)),
    dist_mne = sf::st_distance(geometry, sf::st_as_sf(mne_shapefile)),
    dist_yug = sf::st_distance(geometry, sf::st_as_sf(yug_shapefile)),
    dist_other_country = min(dist_hrv, dist_yug),
    dist_internal_border = sf::st_distance(geometry, internal_lines)
  ) %>%
  as.data.frame() %>%
  dplyr::select(municipality = NAME_3, dist_hrv, dist_srb, dist_mne, dist_yug, dist_other_country,
                dist_internal_border) %>%
  dplyr::mutate(
    # rename death data names to match with post-war standard naming
    municipality = dplyr::case_match(
      municipality,
      "Bosanska Kostajnica" ~ "Kostajnica",
      "Doboj South" ~ "Doboj Jug",
      "Doboj East" ~ "Doboj Istok",
      .default = municipality
    ))

# write formatted data
write.csv(mun_distances, "Formatted Data/municipality_distances.csv", row.names = FALSE)
