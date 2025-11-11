

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(ggplot2)
library(ggrepel)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# deaths by municipality data
death_data <- read.csv("Formatted Data/deaths_per_municipality_formatted.csv")
death_data_postwar_municipalities <- read.csv("Formatted Data/deaths_per_postwar_municipality_formatted.csv")

# minefield data
landmines <- read.csv("Formatted Data/landmines_1997.csv")

# load formatted shapefiles
source("01-Format_shapefiles.R")

# post-war canton shapefile
bih_postwar_canton_shapefile <- sf::read_sf(
  "Shape Files/geoBoundaries-BIH-ADM2-all/geoBoundaries-BIH-ADM2.shp",
  quiet = TRUE
)

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp")

### format data ------------------------------------------------------------------------------------
# death data df with geographic data
deaths_prewar_municipality <- bih_prewar_municipalities_shapefile %>%
  sf::st_make_valid() %>%
  dplyr::select(municipality, geometry) %>%
  dplyr::full_join(death_data, by = "municipality") %>%
  dplyr::select(municipality, deaths_per_population, deaths_count, geometry) %>%
  dplyr::mutate(
    deaths_per_100000 = deaths_per_population * 100000,
    deaths_per_100000_logged = dplyr::case_when(
      deaths_per_100000 == 0 ~ 0,
      deaths_per_100000 > 0 ~ log(deaths_per_100000),
      deaths_per_100000 < 0 ~ log(deaths_per_100000) + 1
    ))

landmines <- landmines %>%
  dplyr::mutate(
    canton = str_replace_all(canton, "[[:space:]]", " "),
    canton = ifelse(canton == "Banja Luka", "Republika Srpska", canton)
    ) %>%
  dplyr::filter(!canton %in% c("Bijeljina", "Doboj", "Foča", "Istočno Sarajevo", "Trebinje",
                               "Vlasenica"))


minefield_data <- bih_postwar_canton_shapefile %>%
  dplyr::select(canton = shapeName, geometry) %>%
  dplyr::mutate(
    canton = str_replace_all(canton, "[[:space:]]", " "),
    canton = dplyr::case_match(
      canton,
      "Zenica-Doboj Canton" ~ "Zenica-Doboj",
      "Brcko District" ~ "Brčko",
      "Posavina Canton" ~ "Posavina",
      "Bosnian-Podrinje Canton Goražde" ~ "Bosnian Podrinje",
      "Herzegovina-Neretva Canton" ~ "Herzegovina-Neretva",
      "Tuzla Canton" ~ "Tuzla",
      "West Herzegovina Canton" ~ "West Herzegovina",
      "Una-Sana Canton" ~ "Una-Sana",
      "Canton 10" ~ "Canton 10",
      "Central Bosnia Canton" ~ "Central Bosnia",
      "Sarajevo Canton" ~ "Sarajevo",
      .default = canton
    ),
    canton = trimws(canton, which = "both")
  ) %>%
  dplyr::full_join(landmines, by = "canton")

### create map -------------------------------------------------------------------------------------

# per capita
deaths_prewar_municipalities_top5_pc <- deaths_prewar_municipality %>%
  dplyr::slice_max(deaths_per_100000, n = 5) %>%
  dplyr::pull(municipality)

deaths_prewar_municipalities_top5_pc_shapefile <- deaths_prewar_municipality %>%
  dplyr::filter(municipality %in% deaths_prewar_municipalities_top5_pc) %>%
  dplyr::mutate(label = paste0(municipality,": ",round(deaths_count)," deaths"))

# legend breaks
original_breaks <- c(150, 1500, 15000) 

logged_break_positions <- case_when(
  original_breaks > 0 ~ log(original_breaks),
  original_breaks == 0 ~ 0,
  .default = NA
  )

final_labels <- scales::label_number(big.mark = ",")(original_breaks[original_breaks >= 0])

deaths_per_capita_prewar_municipalities_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = deaths_prewar_municipality,
    ggplot2::aes(
      fill = deaths_per_100000_logged,
      text = paste0(
        "Municipality: ", municipality, "\n", 
        "Deaths per 100,000 residents: ", round(deaths_per_100000, 1))
      )
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
  ggplot2::geom_sf(
    data = deaths_prewar_municipalities_top5_pc_shapefile,
    color = "black",
    fill = NA,
    size = 0.75
    ) +
  ggrepel::geom_label_repel(
    data = deaths_prewar_municipalities_top5_pc_shapefile,
    ggplot2::aes(label = municipality, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    label.size = 0.25,
    fill = "white",
    colour = "black",
    force = 2,
    nudge_x = 0.5,
    seed = 74
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom")

deaths_per_capita_prewar_municipalities_map

# absolute
deaths_prewar_municipalities_top5_abs <- deaths_prewar_municipality %>%
  dplyr::slice_max(deaths_count, n = 5) %>%
  dplyr::pull(municipality)

deaths_prewar_municipalities_top5_abs_shapefile <- deaths_prewar_municipality %>%
  dplyr::filter(municipality %in% deaths_prewar_municipalities_top5_abs) %>%
  dplyr::mutate(label = paste0(municipality,": ",round(deaths_count)," deaths"))

# legend breaks
original_breaks <- c(250, 1500, 7500) 

logged_break_positions <- case_when(
  original_breaks > 0 ~ log(original_breaks),
  original_breaks == 0 ~ 0,
  .default = NA
)

final_labels <- scales::label_number(big.mark = ",")(original_breaks[original_breaks >= 0])

deaths_absolute_prewar_municipalities_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = deaths_prewar_municipality,
    ggplot2::aes(fill = deaths_per_100000_logged)
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
  ggplot2::geom_sf(
    data = deaths_prewar_municipalities_top5_abs_shapefile,
    color = "black",
    fill = NA,
    size = 0.75
    ) +
  ggrepel::geom_label_repel(
    data = deaths_prewar_municipalities_top5_abs_shapefile,
    ggplot2::aes(label = municipality, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    label.size = 0.25,
    fill = "white",
    colour = "black",
    force = 2,
    nudge_x = 0.5,
    seed = 74
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="bottom")

deaths_absolute_prewar_municipalities_map


### Minefields -------------------------------------------------------------------------------------
minefield_density_canton_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = minefield_data,
    ggplot2::aes(
      fill = minefields_per_sq_km,
      text = paste0(
        "Canton: ", canton, "\n", 
        "Percent Yugoslavs: ", round(minefields_per_sq_km, 2)
      ))
  ) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkred", na.value = "grey50") +
  ggplot2::labs(fill = "Estimated\nMinefields\nper sq. km") +
  ggplot2::theme_void()

minefield_density_canton_map <- plotly::ggplotly(
  minefield_density_canton_map,
  tooltip = c("text")
)

minefield_density_canton_map

