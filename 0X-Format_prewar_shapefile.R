
# Format Andreas Berger's pre-war municipality shapefile with appended municipality names.

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(readxl)
library(mapview)
library(tidyverse)


### load data --------------------------------------------------------------------------------------
# pre-war municipalities shapefile
bih_prewar_municipalities_shapefile <- sf::st_read("Shape Files/prewar municipalities/AdminBound/BiH_municipalities.shp")

# crosswalk for shapefile Id value and name of municipality
bih_prewar_municipality_id_crosswalk <- readxl::read_xlsx("Data/bih_prewar_municipality_id_crosswalk.xlsx")

# post-war municipality shapefile
bih_postwar_municipalities_shapefile <- sf::st_read("Shape Files/admin3/BIH_adm3.shp")

### format shapefile -------------------------------------------------------------------------------
bih_prewar_municipalities_shapefile <- bih_prewar_municipalities_shapefile %>%
  dplyr::left_join(bih_prewar_municipality_id_crosswalk, by = "Id") %>%
  dplyr::select(id = "Id", "municipality" = "Name", geometry) %>%
  dplyr::mutate(
    mun_area = sf::st_area(geometry),
    mun_perimeter = sf::st_perimeter(geometry)
  )




# bih_prewar_shapefile <- sf::st_read("Shape Files/prewar municipalities/AdminBound/BiH_munic_area.shp")
# 
# plot_one_province <- function(shapefile = bih_prewar_shapefile, munid = 0){
#   
#   df <- shapefile %>%
#     dplyr::mutate(
#       color = ifelse(Id == munid, "blue", "gray")
#     )
#   
#   
#   map <- ggplot2::ggplot(data = df,
#                          ggplot2::aes(fill = color)) +
#     ggplot2::geom_sf() +
#     # ggplot2::scale_fill_manual(values = color_palette) +
#     ggplot2::theme_void()
# 
#   return(map)
#   
# }
# 
# plot_one_province(munid = 31)
# 
# mapview(bih_prewar_shapefile)

