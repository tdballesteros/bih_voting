
### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(modelsummary)
library(plotly)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# Costello and Moro
cm_deaths <- read.csv("Formatted Data/deaths_per_municipality_formatted.csv")

# UCDP GED
ucdp_deaths <- read.csv("Formatted Data/ucdp_deaths_prewar_municipality_formatted.csv")

# 1991 census data
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# load formatted prewar shapefile
bih_prewar_municipalities_shapefile <- sf::read_sf(
  "Shape Files/bih_prewar_municipalities_shapefile_formatted.shp",
  quiet = TRUE
)

# IEBF shapefile
iebf_shapefile <- sf::read_sf("Shape Files/internal_entity_division_shapefile.shp", quiet = TRUE)

### set color scheme -------------------------------------------------------------------------------
cm_color <- "#603055"
ucdp_color <- "#990000"

### format and merge data --------------------------------------------------------------------------
cm_deaths <- cm_deaths %>%
  dplyr::select(municipality, deaths_per_population_cm = deaths_per_population,
                deaths_count_cm = deaths_count)

ucdp_deaths = ucdp_deaths %>%
  dplyr::select(municipality, count_of_events_ucdp = count_of_events, deaths_count_best_ucdp = best,
                deaths_count_low_ucdp = low, deaths_count_high_ucdp = high,
                deaths_per_100000_ucdp = deaths_per_100000,
                deaths_per_100000_low_ucdp = deaths_per_100000_low,
                deaths_per_100000_high_ucdp = deaths_per_100000_high)

census_data_1991 <- census_data_1991 %>%
  dplyr::select(municipality, population = total, count_muslims = muslims, count_croats = croats,
                count_serbs = serbs, count_yugoslavs = yugoslavs, count_other = other,
                percent_muslims, percent_croats, percent_serbs, percent_yugoslavs, percent_other)

death_comparison_df <- dplyr::full_join(cm_deaths, ucdp_deaths, by = "municipality") %>%
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  # drop unassigned deaths from UCDP dataset
  dplyr::filter(!is.na(municipality)) %>%
  dplyr::mutate(
    deaths_per_population_ucdp = deaths_count_best_ucdp / population,
    deaths_count_estimate_diff = deaths_count_cm - deaths_count_best_ucdp,
    deaths_count_estimate_diff_direction = ifelse(deaths_count_estimate_diff >= 0, "lower", "higher"),
    deaths_count_cm_log = ifelse(deaths_count_cm > 0, log(deaths_count_cm), 0),
    deaths_count_best_ucdp_log = ifelse(deaths_count_best_ucdp > 0, log(deaths_count_best_ucdp), 0),
    deaths_count_low_ucdp_log = ifelse(deaths_count_low_ucdp > 0, log(deaths_count_low_ucdp), 0),
    deaths_count_high_ucdp_log = ifelse(deaths_count_high_ucdp > 0, log(deaths_count_high_ucdp), 0),
    perc_of_deaths_cm = deaths_count_cm / sum(deaths_count_cm),
    perc_of_deaths_best_ucdp = deaths_count_best_ucdp / sum(deaths_count_best_ucdp),
    perc_of_deaths_low_ucdp = deaths_count_low_ucdp / sum(deaths_count_low_ucdp),
    perc_of_deaths_high_ucdp = deaths_count_high_ucdp / sum(deaths_count_high_ucdp),
    diff_in_perc = perc_of_deaths_cm - perc_of_deaths_best_ucdp) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ucdp_perc_lower = min(perc_of_deaths_low_ucdp, perc_of_deaths_high_ucdp),
    ucdp_perc_upper = max(perc_of_deaths_low_ucdp, perc_of_deaths_high_ucdp)
    ) %>%
  dplyr::arrange(deaths_count_cm) %>%
  dplyr::mutate(
    # row_order = nrow(death_comparison_df) - row_number() + 1,
    municipality = factor(municipality, levels = municipality)
    )

# plot absolute deaths
death_counts_comparison_plot <- ggplot2::ggplot(data = death_comparison_df) +
  ggplot2::geom_segment(
    ggplot2::aes(x = deaths_count_cm_log, xend = deaths_count_best_ucdp_log,
                 y = municipality, yend = municipality,
                 color = deaths_count_estimate_diff_direction
    )) +
  theme_minimal() +
  ggplot2::scale_color_manual(values = c("C&M" = cm_color, "UCDP" = ucdp_color, "lower" = "darkgreen", "higher" = "darkred")) +
  
  ggplot2::geom_point(
    ggplot2::aes(x = deaths_count_cm_log, y = municipality, size = deaths_per_population_cm, color = "C&M")
    ) +
  ggplot2::geom_point(
    ggplot2::aes(x = deaths_count_best_ucdp_log, y = municipality, size = deaths_per_population_ucdp, color = "UCDP")
    )

death_counts_comparison_plot

##########################################

# plot diff in percent of total deaths
death_perc_comparison_df <- death_comparison_df %>%
  dplyr::filter(abs(diff_in_perc) > 0.01) %>%
  dplyr::rename(Municipality = municipality) %>%
  dplyr::rename(
    `Percent of Deaths, C&M` = perc_of_deaths_cm,
    `Percent of Deaths, UCDP` = perc_of_deaths_best_ucdp,
    `Deaths per Population, C&M` = deaths_per_population_cm,
    `Deaths per Population, UCDP` = deaths_per_population_ucdp
  )

death_perc_of_total_plot <- ggplot2::ggplot(data = death_perc_comparison_df) +
  
  # ucdp low and high range
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = ucdp_perc_lower, xmax = ucdp_perc_upper, y = Municipality), 
                 width = 0.4, 
                 color = "black",
                 alpha = 0.6
    ) +
  
  # dumbells
  ggplot2::geom_segment(
    ggplot2::aes(x = `Percent of Deaths, C&M`, xend = `Percent of Deaths, UCDP`,
                 y = Municipality, yend = Municipality,
    )) +
  
  ggplot2::scale_color_manual(
    name = "Data Source",
    values = c("Costalli & Moro est." = cm_color, "UCDP est." = ucdp_color)
    ) +
  
  ggplot2::scale_size_continuous(name = "Deaths per\nPre-War Population") +
  
  ggplot2::guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  ) +

  # C&M data points
  ggplot2::geom_point(
    ggplot2::aes(x = `Percent of Deaths, C&M`, y = Municipality, size = `Deaths per Population, C&M`, color = "Costalli & Moro est.")
  ) +
  
  # UCDP data points
  ggplot2::geom_point(
    ggplot2::aes(x = `Percent of Deaths, UCDP`, y = Municipality, size = `Deaths per Population, UCDP`, color = "UCDP est.")
  ) +
  
  ggplot2::labs(x = "Estimated Percentage of Deaths", y = "Municipality") +
  # ggplot2::theme(
  #   legend.position = "bottom",
  #   legend.box = "horizontal"
  #   ) + 
  # ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(labels = scales::percent)


death_perc_of_total_plot

plotly <- plotly::ggplotly(
  death_perc_of_total_plot,
  tooltip = c("Municipality", "Percent of Deaths, C&M", "Percent of Deaths, UCDP",
              "Deaths per Population, C&M", "Deaths per Population, UCDP")
  )
plotly


#### Differences in Percent of Deaths - Map --------------------------------------------------------
death_perc_comparison_df_tmp <- death_comparison_df %>%
  dplyr::mutate(diff_in_perc = 100 * diff_in_perc) %>%
  dplyr::rename(
    Municipality = municipality,
    `Percent of Deaths, C&M` = perc_of_deaths_cm,
    `Percent of Deaths, UCDP` = perc_of_deaths_best_ucdp,
    `Deaths per Population, C&M` = deaths_per_population_cm,
    `Deaths per Population, UCDP` = deaths_per_population_ucdp,
    `Difference in Percent\nof Total Deaths` = diff_in_perc
  )

death_perc_comparison_df_shapefile <- bih_prewar_municipalities_shapefile %>%
  dplyr::rename(Municipality = mncplty) %>%
  dplyr::full_join(death_perc_comparison_df_tmp, by = "Municipality")

death_perc_comparison_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = death_perc_comparison_df_shapefile,
    ggplot2::aes(
      fill = `Difference in Percent\nof Total Deaths`,
      text = paste0(
        "Municipality: ", Municipality, "\n", 
        "Difference in Percent of Total Deaths: ", round(`Difference in Percent\nof Total Deaths`, 1), "%"
        )
      )
    ) +
  ggplot2::scale_fill_gradient2(
    low = cm_color,
    mid = "white",
    high = ucdp_color,
    na.value = "grey70"
  ) +
  ggplot2::geom_sf(data = iebf_shapefile, color = "black", size = 0.4) +
  ggplot2::theme_void()

death_perc_comparison_map

death_perc_comparison_map_plotly <- plotly::ggplotly(
  death_perc_comparison_map,
  tooltip = c("text")
)
death_perc_comparison_map_plotly

### v2 -------
death_perc_comparison_df2 <- death_comparison_df %>%
  dplyr::filter(abs(diff_in_perc) > 0.01) %>%
  dplyr::rename(Municipality = municipality) %>%
  dplyr::rename(
    `Percent of Deaths, C&M` = perc_of_deaths_cm,
    `Percent of Deaths, UCDP` = perc_of_deaths_best_ucdp,
    `Deaths per Population, C&M` = deaths_per_population_cm,
    `Deaths per Population, UCDP` = deaths_per_population_ucdp,
    `Deaths per Population, UCDP low` = ucdp_perc_lower,
    `Deaths per Population, UCDP high` = ucdp_perc_upper
  ) %>%
  dplyr::select(Municipality, `Percent of Deaths, C&M`, `Percent of Deaths, UCDP`,
                `Deaths per Population, C&M`, `Deaths per Population, UCDP`,
                `Deaths per Population, UCDP low`, `Deaths per Population, UCDP high`) %>%
  tidyr::pivot_longer(2:3, names_to = "Data Source", values_to = "Percent of Deaths") %>%
  dplyr::mutate(
    `Deaths per Population` = dplyr::case_when(
      `Data Source` == "Percent of Deaths, C&M" ~ `Deaths per Population, C&M`,
      `Data Source` == "Percent of Deaths, UCDP" ~ `Deaths per Population, UCDP`
    ),
    `Percent of Deaths` = as.numeric(`Percent of Deaths`),
    `Deaths per Population` = as.numeric(`Deaths per Population`)
  ) %>%
  dplyr::select(Municipality, `Data Source`, `Percent of Deaths`, `Deaths per Population`,
                `Deaths per Population, UCDP low`, `Deaths per Population, UCDP high`)

