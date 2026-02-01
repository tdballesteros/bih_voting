
# This file formats Andreas Berger's pre-war and the Global Administrative Areas 2015 (v2.8)'s
# post-war municipalities shapefiles.

# Due to issues saving files using special characters in the Serbo-Croatian Latin alphabet, this
# script is sourced at the start of scripts needing the formatted shapefiles. These shapefiles are
# exported as part of this script, but municipality names contain formatting errors.

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(readxl)
library(mapview)
library(units)
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
  "Shape Files/geoBoundaries-BIH-ADM3-all/geoBoundaries-BIH-ADM3.shp",
  quiet = TRUE
  )

# post-war municipality data
postwar_municipality_data <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")

### format postwar shapefile -----------------------------------------------------------------------
bih_postwar_municipalities_shapefile <- bih_postwar_municipalities_shapefile %>%
  sf::st_make_valid() %>%
  dplyr::rename(municipality = shapeName) %>%
  dplyr::mutate(
    municipality = dplyr::case_when(
      municipality == "Brcko District" ~ "Brčko",
      municipality == "Centar" ~ "Sarajevo Centar",
      municipality == "Doboj East" ~ "Doboj Istok",
      municipality == "Kneževo" ~ "Skender Vakuf",
      municipality == "Kozarska Dubica" ~ "Dubica",
      municipality == "Kupra na Uni" ~ "Krupa na Uni",
      municipality == "Kupres" ~ "Kupres-RS",
      municipality == "Kupres (BiH)" ~ "Kupres-FBIH",
      municipality == "Novi Grad" & shapeID == "43093233B94913651340481" ~ "Sarajevo Novi Grad",
      municipality == "Republika Srpska" ~ "Višegrad",
      municipality == "Stari Grad" ~ "Sarajevo Stari Grad",
      municipality == "Trnovo (BiH)" ~ "Trnovo-FBIH",
      municipality == "Trnovo (RS)" ~ "Trnovo-RS",
      municipality == "Ustiprača" ~ "Novo Goražde",
      .default = municipality
    ),
    mun_area = units::set_units(sf::st_area(geometry), km^2),
    mun_perimeter = units::set_units(sf::st_perimeter(geometry), km)
  ) %>%
  dplyr::full_join(postwar_municipality_data, by = "municipality") %>%
  dplyr::select(municipality, canton, entity, sarajevo_district = `sarajevo district`,
                split_by_iebl = `split by line`, mun_area, mun_perimeter, geometry) %>%
  dplyr::arrange(municipality)

# write data
sf::write_sf(bih_postwar_municipalities_shapefile,
             "Shape Files/bih_postwar_municipalities_shapefile_formatted.shp",
             layer_options = "ENCODING=UTF-8")

### format prewar shapefile ------------------------------------------------------------------------
bih_prewar_municipalities_shapefile <- bih_prewar_municipalities_shapefile %>%
  dplyr::left_join(bih_prewar_municipality_id_crosswalk, by = "Id") %>%
  dplyr::select(id = "Id", "municipality" = "Name", geometry) %>%
  sf::st_make_valid()

# Add approximated Sarajevo Centar and Sarajevo Novi Grad municipalities from the post-war
# municipalities shapefile

# pull post-war Sarajevo Centar and Sarajevo Novi Grad shapes
bih_sarajevo_postwar_centar <- bih_postwar_municipalities_shapefile %>%
  dplyr::filter(municipality == "Sarajevo Centar") %>%
  dplyr::mutate(id = 108) %>%
  dplyr::select(id, municipality, geometry)

bih_sarajevo_postwar_novi_grad <- bih_postwar_municipalities_shapefile %>%
  dplyr::filter(municipality == "Sarajevo Novi Grad") %>%
  dplyr::mutate(id = 109) %>%
  dplyr::select(id, municipality, geometry)

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
