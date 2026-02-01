
### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(modelsummary)
library(plotly)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# 1991 census data
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# 2013 census data
census_data_2013 <- read.csv("Formatted Data/census_data_2013_formatted.csv")

# municipality reorg crosswalk
municipality_reorg <- readxl::read_xlsx("Data/bih_municipality_reorg_crosswalk.xlsx")

### format and merge data --------------------------------------------------------------------------
fractionalization_1991 <- census_data_1991 %>%
  dplyr::select(municipality, population_1991 = total, fractionalization_1991 = fractionalization,
                polarization_1991 = polarization)

fractionalization_2013 <- census_data_2013 %>%
  dplyr::select(municipality, population_2013 = population,
                fractionalization_2013 = fractionalization, polarization_2013 = polarization)

fractionalization_df <- municipality_reorg %>%
  dplyr::full_join(fractionalization_1991, dplyr::join_by("Prewar Municipality" == "municipality")) %>%
  dplyr::full_join(fractionalization_2013, dplyr::join_by("Postwar Municipality" == "municipality"))

### graph - non-modified municipalities ------------------------------------------------------------

fractionalization_df_nonmod <- fractionalization_df %>%
  # drop municipalities with inter-war border modificaitons
  dplyr::filter(Modified == 0) %>%
  dplyr::rename(
    Municipality = `Postwar Municipality`,
    `Population, 1991` = population_1991,
    `Population, 2013` = population_2013,
    `Fractionalization, 1991` = fractionalization_1991,
    `Fractionalization, 2013` = fractionalization_2013,
    `Polarization, 1991` = polarization_1991,
    `Polarization, 2013` = polarization_2013
    ) %>%
  dplyr::mutate(
    `Change in Fractionalization` = `Fractionalization, 2013` - `Fractionalization, 1991`,
    `Change in Polarization` = `Polarization, 2013` - `Polarization, 1991`
    ) %>%
  dplyr::arrange(desc(`Change in Fractionalization`)) %>%
  dplyr::mutate(
    Municipality = factor(Municipality, levels = Municipality)
  )

fractionalization_change_graph <- ggplot2::ggplot(data = fractionalization_df_nonmod) +
  
  # dumbells
  ggplot2::geom_segment(
    ggplot2::aes(x = `Fractionalization, 1991`, xend = `Fractionalization, 2013`,
                 y = Municipality, yend = Municipality,
    )) +
  
  ggplot2::scale_color_manual(
    name = "Data Source",
    values = c("1991" = "#7AA0C4", "2013" = "#5C4059")
  ) +
  
  ggplot2::scale_size_continuous(name = "Pre-War Population") +
  
  ggplot2::guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  ) +
  
  # 1991 data points
  ggplot2::geom_point(
    ggplot2::aes(x = `Fractionalization, 1991`, y = Municipality, size = `Population, 1991`, color = "1991")
  ) +
  
  # 2013 data points
  ggplot2::geom_point(
    ggplot2::aes(x = `Fractionalization, 2013`, y = Municipality, size = `Population, 2013`, color = "2013")
  ) +
  
  ggplot2::labs(x = "Ethnic Fractionalization Index Score", y = "Municipality") #+
  # ggplot2::theme(
  #   legend.position = "bottom",
  #   legend.box = "horizontal"
  #   ) + 
  # ggplot2::theme_minimal() +
  # ggplot2::scale_x_continuous(labels = scales::percent)


fractionalization_change_graph

