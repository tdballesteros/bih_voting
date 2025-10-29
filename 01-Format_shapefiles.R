
# This file formats Andreas Berger's pre-war and the Global Administrative Areas 2015 (v2.8)'s
# post-war municipalities shapefiles.

# Due to issues saving files using special characters in the Serbo-Croatian Latin alphabet, this
# script is sourced at the start of scripts needing the formatted shapefiles. These shapefiles are
# exported as part of this script, but municipality names contain formatting errors.

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(readxl)
library(mapview)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# pre-war municipalities shapefile
bih_prewar_municipalities_shapefile <- sf::st_read(
  "Shape Files/prewar municipalities/AdminBound/BiH_municipalities.shp",
  quiet = TRUE
  )

# crosswalk for shapefile Id value and name of municipality
bih_prewar_municipality_id_crosswalk <- readxl::read_xlsx(
  "Data/bih_prewar_municipality_id_crosswalk.xlsx"
  )

# post-war municipality shapefile
bih_postwar_municipalities_shapefile <- sf::st_read(
  "Shape Files/admin3/BIH_adm3.shp",
  quiet = TRUE
  )

# postwar_alt <- sf::st_read("Shape Files/geoBoundaries-BIH-ADM3-all/geoBoundaries-BIH-ADM3.shp")
# 
# mapview(postwar_alt)
# mapview(bih_postwar_municipalities_shapefile)


# usora municipality shapefile
bih_usora_shapefile <- sf::st_read(
  "Shape Files/bih_usora_tesenj.shp",
  quiet = TRUE
  )

### format prewar shapefile ------------------------------------------------------------------------
bih_prewar_municipalities_shapefile <- bih_prewar_municipalities_shapefile %>%
  dplyr::left_join(bih_prewar_municipality_id_crosswalk, by = "Id") %>%
  dplyr::select(id = "Id", "municipality" = "Name", geometry) %>%
  sf::st_make_valid()

# Add approximated Sarajevo Centar and Sarajevo Novi Grad municipalities from the post-war
# municipalities shapefile

# pull post-war Sarajevo Centar and Sarajevo Novi Grad shapes
bih_sarajevo_postwar_centar <- bih_postwar_municipalities_shapefile %>%
  dplyr::filter(NAME_3 == "Centar Sarajevo") %>%
  dplyr::select(municipality = NAME_3) %>%
  dplyr::mutate(
    municipality = "Sarajevo Centar",
    id = 108
  )

bih_sarajevo_postwar_novi_grad <- bih_postwar_municipalities_shapefile %>%
  dplyr::filter(NAME_3 == "Novi Grad Sarajevo") %>%
  dplyr::select(municipality = NAME_3) %>%
  dplyr::mutate(
    municipality = "Sarajevo Novi Grad",
    id = 109
  )

# remove overlap with pre-war shapefile
bih_prewar_removed_shapes <- sf::st_difference(bih_prewar_municipalities_shapefile,
                                               bih_sarajevo_postwar_centar)

bih_prewar_removed_shapes <- sf::st_difference(bih_prewar_removed_shapes,
                                               bih_sarajevo_postwar_novi_grad)

# append post-war shapefiles
bih_prewar_municipalities_shapefile <- bih_prewar_removed_shapes %>%
  dplyr::select(id, municipality, geometry) %>%
  rbind(bih_sarajevo_postwar_centar, bih_sarajevo_postwar_novi_grad) %>%
  dplyr::mutate(
    mun_area = units::set_units(sf::st_area(geometry), km^2),
    mun_perimeter = units::set_units(sf::st_perimeter(geometry), km),
  ) %>%
  dplyr::select(id, municipality, mun_area, mun_perimeter, geometry)

# write data
sf::write_sf(bih_prewar_municipalities_shapefile,
             "Shape Files/bih_prewar_municipalities_shapefile_formatted.shp",
             layer_options = "ENCODING=UTF-8")

### format postwar shapefile -----------------------------------------------------------------------
bih_tesenj_entry <- bih_postwar_municipalities_shapefile %>%
  dplyr::filter(NAME_3 == "Tešan/ Usora")

bih_tesenj_shape <- sf::st_difference(bih_tesenj_entry, bih_usora_shapefile) %>%
  dplyr::mutate(NAME_3 = "Tešanj") %>%
  dplyr::select(-c(id, Municipali))

bih_postwar_municipalities_shapefile_formatted <- bih_postwar_municipalities_shapefile %>%
  # drop Tešanj from the shapefile and add custom shape Tešanj
  dplyr::filter(NAME_3 != "Tešan/ Usora") %>%
  rbind(bih_tesenj_shape) %>%
  dplyr::mutate(NAME_3 = dplyr::case_when(
    NAME_3 == "Kupres" & NAME_1 == "Federacija Bosna i Hercegovina" ~ "Kupres-FBIH",
    NAME_3 == "Kupres" & NAME_1 == "Repuplika Srpska" ~ "Kupres-RS",
    NAME_3 == "Mostar" & NAME_1 == "Federacija Bosna i Hercegovina" ~ "Mostar",
    NAME_3 == "Mostar" & NAME_1 == "Repuplika Srpska" ~ "Istočni Mostar",
    NAME_3 == "Trnovo" & NAME_1 == "Federacija Bosna i Hercegovina" ~ "Trnovo-FBIH",
    NAME_3 == "Trnovo" & NAME_1 == "Repuplika Srpska" ~ "Trnovo-RS",
    # adjust names
    NAME_3 == "Bosanska Gradiška" ~ "Gradiška",
    NAME_3 == "Bosanska Kostajnica" ~ "Kostajnica",
    NAME_3 == "Bosanski Brod" ~ "Brod",
    NAME_3 == "Centar Sarajevo" ~ "Sarajevo Centar",
    NAME_3 == "Doboj East" ~ "Doboj Istok",
    NAME_3 == "Doboj South" ~ "Doboj Jug",
    NAME_3 == "East New Sarajevo" ~ "Istočno Novo Sarajevo",
    NAME_3 == "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
    NAME_3 == "Skender Vakuf / Kneževo" ~ "Skender Vakuf",
    NAME_3 == "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
    .default = NAME_3
  ))

#### add Usora -------------------------------------------------------------------------------------
sf::sf_use_s2(FALSE) 

country_total <- bih_postwar_municipalities_shapefile %>%
  sf::st_union()

country_hole <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_union()

bih_usora_entry <- sf::st_difference(country_total, country_hole) %>%
  sf::st_sf(
    ID_0 = 30,
    ISO = "BIH",
    NAME_0 = "Bosnia and Herzegovina",
    ID_1 = 2,
    NAME_1 = "Federacija Bosna i Hercegovina",
    ID_2 = 11,
    NAME_2 = "Zenica-Doboj",
    ID_3 = 150,
    NAME_3 = "Usora",
    CCN_3 = 0,
    CCA_3 = NA,
    TYPE_3 = "Opčine",
    ENGTYPE_3 = "Commune",
    NL_NAME_3 = NA,
    VARNAME_3 = NA
    )

bih_usora_entry <- bih_usora_entry %>%
  dplyr::rename(geometry = ncol(bih_usora_entry))

bih_postwar_municipalities_shapefile_formatted <- bih_postwar_municipalities_shapefile_formatted %>%
  rbind(bih_usora_entry) %>%
  sf::st_make_valid()

sf::sf_use_s2(TRUE)

bih_postwar_municipalities_shapefile_formatted <- bih_postwar_municipalities_shapefile_formatted %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    mun_area = units::set_units(sf::st_area(geometry), km^2),
    mun_perimeter = units::set_units(sf::st_perimeter(geometry), km)
    ) %>%
  dplyr::select(municipality = NAME_3, canton = NAME_2, entity = NAME_1, mun_area, mun_perimeter,
                geometry)

# write data
sf::write_sf(bih_postwar_municipalities_shapefile_formatted,
             "Shape Files/bih_postwar_municipalities_shapefile_formatted.shp",
             layer_options = "ENCODING=UTF-8")
