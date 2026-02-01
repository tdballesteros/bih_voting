
# This script assesses the municipality-level ethnic fractionalization scores versus community-level
# scores.

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# municipality-level 1991 census data
census_data_1991_municipality <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# community-level 1991 census data
census_data_1991_community <- read.csv("Formatted Data/census_data_1991_community_formatted.csv")

### identify top/bottom municipalities -------------------------------------------------------------
municipality_top10_fractionalization <- census_data_1991_municipality %>%
  dplyr::slice_max(fractionalization, n = 10) %>%
  dplyr::pull(municipality)

municipality_bottom10_fractionalization <- census_data_1991_municipality %>%
  dplyr::slice_min(fractionalization, n = 10) %>%
  dplyr::arrange(desc(row_number())) %>%
  dplyr::pull(municipality)

# calculate bosnia-wide fractionalization score
bih_fractionalization <- census_data_1991_municipality %>%
  dplyr::summarise(across(c(muslims, serbs, croats, yugoslavs, other), sum)) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(count = 1) %>%
  dplyr::mutate(
    perc = count / sum(count),
    perc_sq = perc^2
  )

bih_fractionalization <- sum(bih_fractionalization$perc_sq)

municipality_fractionalization <- census_data_1991_municipality %>%
  dplyr::select(municipality, fractionalization, total) %>%
  dplyr::arrange(desc(fractionalization)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::filter(municipality %in% c(municipality_top10_fractionalization,
                                    municipality_bottom10_fractionalization)) %>%
  dplyr::arrange(desc(fractionalization)) %>%
  dplyr::mutate(row_order = row_number()) %>%
  # tibble::add_row(
  #   municipality = "Bosnia and Herzegovina total",
  #   fractionalization = bih_fractionalization,
  #   rank = NA,
  #   row_order = 10.5
  # ) %>%
  dplyr::mutate(
    row_order = 21 - row_order,
    group = "Municipality")

municipality_fractionalization_row_order <- municipality_fractionalization %>%
  dplyr::select(municipality, row_order, rank)

### pull and append community index values ---------------------------------------------------------
community_fractionalization <- census_data_1991_community %>%
  dplyr::filter(municipality %in% c(municipality_top10_fractionalization,
                                    municipality_bottom10_fractionalization)
  ) %>%
  dplyr::left_join(municipality_fractionalization_row_order, by = "municipality") %>%
  dplyr::select(row_order, rank, municipality, fractionalization, total) %>%
  dplyr::mutate(group = "Community")


### bind data --------------------------------------------------------------------------------------
fractionalization_df <- rbind(municipality_fractionalization, community_fractionalization) %>%
  dplyr::mutate(total_log = ifelse(group == "Community", log(total)/25, 1)) %>%
  dplyr::arrange(group)

fract_graph <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = fractionalization_df,
    ggplot2::aes(x = fractionalization,
                 y = row_order,
                 size = total_log,
                 color = group)
  ) +
  ggplot2::scale_color_manual(values = c("black", "blue")) +
  ggplot2::lims(x = c(0, 1))

fract_graph

