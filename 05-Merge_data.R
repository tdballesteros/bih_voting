
# Create full datasets for pre- and post-war municipalities

### load libraries ---------------------------------------------------------------------------------
library(sf)
library(readxl)
library(openxlsx)
library(dplyr)

### load data --------------------------------------------------------------------------------------
#### pre-war data ----------------------------------------------------------------------------------
# shapefile
bih_prewar_municipalities_shapefile <- sf::read_sf(
  "Shape Files/bih_prewar_municipalities_shapefile_formatted.shp",
  quiet = TRUE
  )

# 1991 census data
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# death data
death_data <- read.csv("Formatted Data/deaths_per_municipality_formatted.csv")

# municipality distances
mun_distances_prewar <- read.csv("Formatted Data/prewar_municipality_distances.csv")

# pre-war x post-war municipality crosswalk
municipality_crosswalk <- readxl::read_xlsx("Data/bih_municipality_reorg_crosswalk.xlsx")

#### post-war data ---------------------------------------------------------------------------------
# shapefile
bih_postwar_municipalities_shapefile <- sf::read_sf(
  "Shape Files/bih_postwar_municipalities_shapefile_formatted.shp",
  quiet = TRUE
)

# 2013 census data
census_data_2013 <- read.csv("Formatted Data/census_data_2013_formatted.csv")

# nationalist/non-nationalist 1997 vote proportions
nationalist_votes <- read.csv("Formatted Data/nationalist_votes_1997.csv")

# 1997 voting location
voting_location <- read.csv("Formatted Data/voting_location_1997.csv")

# municipality distances
mun_distances_postwar <- read.csv("Formatted Data/postwar_municipality_distances.csv")

# post-war municipality metadata
postwar_metadata <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")

# minefields
landmines <- read.csv("Formatted Data/landmines_1997.csv")

### merge data -------------------------------------------------------------------------------------
# prewar
prewar_data <- bih_prewar_municipalities_shapefile %>%
  dplyr::rename(municipality = mncplty,
                mun_area = mun_are,
                mun_perimeter = mn_prmt) %>%
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  dplyr::full_join(death_data, by = "municipality") %>%
  dplyr::full_join(mun_distances_prewar, by = "municipality") %>%
  dplyr::arrange(municipality) %>%
  as.data.frame() %>%
  dplyr::select(
    Municipality = municipality, `Municipality Area, Pre-War` = mun_area,
    `Municipality Perimeter, Pre-War` = mun_perimeter, `Population, 1991` = total,
    `Muslim Population, 1991` = muslims, `Croat Population, 1991` = croats,
    `Serb Population, 1991` = serbs, `Yugoslav Population, 1991` = yugoslavs,
    `Other Population, 1991` = other, `Muslim Population Percentage, 1991` = percent_muslims,
    `Croat Population Percentage, 1991` = percent_croats,
    `Serb Population Percentage, 1991` = percent_serbs,
    `Yugoslav Population Percentage, 1991` = percent_yugoslavs,
    `Other Population Percentage, 1991` = percent_other,
    `Ethnic Fractionalization, 1991` = fractionalization,
    `Ethnic Polarization, 1991` = polarization,
    `Percent of Population, 1991` = percent_total_population,
    `Deaths per Population` = deaths_per_population, `Number of Deaths` = deaths_count,
    `Ethnic Dominance` = ethnic_dominance_cm, `Distance to Croatia, Pre-War` = dist_hrv,
    `Distance to Serbia, Pre-War` = dist_srb, `Distance to Montenegro, Pre-War` = dist_mne,
    `Distance to Yugoslavia, Pre-War` = dist_yug,
    `Distance to Another Country, Pre-War` = dist_other_country,
    `Distance to IEBL, Pre-War` = dist_internal_border
  )

# write formatted data
openxlsx::write.xlsx(
  prewar_data,
  "Formatted Data/data_prewar.xlsx"
)

# postwar
postwar_data <- bih_postwar_municipalities_shapefile %>%
  as.data.frame() %>%
  dplyr::select(-c(canton, entity, srjv_ds, splt_b_)) %>%
  dplyr::full_join(postwar_metadata, dplyr::join_by("mncplty" == "municipality")) %>%
  dplyr::rename(municipality = mncplty,
                sarajevo_district = `sarajevo district`,
                split_by_iebl = `split by line`,
                mun_area = mun_are,
                mun_perimeter = mn_prmt) %>%
  dplyr::full_join(census_data_2013, by = "municipality") %>%
  dplyr::full_join(nationalist_votes, by = "municipality") %>%
  dplyr::full_join(voting_location, by = "municipality") %>%
  dplyr::full_join(mun_distances_postwar, by = "municipality") %>%
  dplyr::left_join(landmines, by = "canton") %>%
  dplyr::arrange(municipality) %>%
  dplyr::select(
    Municipality = municipality, Canton = canton, Entity = entity,
    `Sarajevo District` = sarajevo_district, `Split by IEBL` = split_by_iebl,
    `Municipality Area, Post-War` = mun_area, `Municipality Perimeter, Post-War` = mun_perimeter,
    `Population, 2013` = total, `Bosniak Population, 2013` = bosniak,
    `Croat Population, 2013` = croat, `Serb Population, 2013` = serb,
    `Not Declared Population, 2013` = not.declared, `Other Population, 2013` = other,
    `No Answer Population, 2013` = no.answer,
    `Bosniak Population Percentage, 2013` = percent_bosniaks,
    `Croat Population Percentage, 2013` = percent_croats,
    `Serb Population Percentage, 2013` = percent_serbs,
    `Not Declared Population Percentage, 2013` = percent_not_declared,
    `Other Population Percentage, 2013` = percent_other,
    `No Answer Population Percentage, 2013` = percent_no_answer,
    `Ethnic Fractionalization, 2013` = fractionalization,
    `Ethnic Polarization, 2013` = polarization,
    `Percent of Population, 2013` = percent_total_population,
    `Proportion of Votes for Nationalist Parties, 1997` = Nationalist,
    `Proportion of Votes for Non-Nationalist Parties, 1997` = Non.Nationalist,
    `Proportion of Votes for Independent Parties, 1997` = Independent,
    `Proportion of Votes for Other Parties, 1997` = Other,
    `Proportion of Invalid Ballots, 1997` = Invalid.Ballot,
    `Number of Votes Cast In-District, 1997` = votes_cast_in_district,
    `Number of Votes Cast Out-District, 1997` = votes_cast_out_district,
    `Total Votes Cast, 1997` = total_votes_cast,
    `Proportion of Votes Cast In-District, 1997` = perc_cast_in,
    `Proportion of Votes Cast Out-District, 1997` = perc_cast_out,
    `Distance to Croatia, Post-War` = dist_hrv, `Distance to Serbia, Post-War` = dist_srb,
    `Distance to Montenegro, Post-War` = dist_mne, `Distance to Yugoslavia, Post-War` = dist_yug,
    `Distance to Another Country, Post-War` = dist_other_country,
    `Distance to IEBL, Post-War` = dist_internal_border,
    `Minefield Density` = minefields_per_sq_km
  ) %>%
  dplyr::mutate(
    # code binaries for entities - Brčko is coded as belonging to both
    `Republika Srpska Municipality` = ifelse(Entity %in% c("Republika Srpska", "Brčko"), 1, 0),
    `Federation Municipality` = ifelse(Entity != "Republika Srpska", 1, 0),
  )

# write formatted data
openxlsx::write.xlsx(
  postwar_data,
  "Formatted Data/data_postwar.xlsx"
)

# pre-war data assigned to post-war municipalities
prewar_data_assigned_to_postwar <- bih_postwar_municipalities_shapefile %>%
  as.data.frame() %>%
  dplyr::select(Municipality = mncplty) %>%
  dplyr::full_join(municipality_crosswalk,
                   dplyr::join_by("Municipality" == "Postwar Municipality")) %>%
  dplyr::full_join(prewar_data, dplyr::join_by("Prewar Municipality" == "Municipality"))
  
# write formatted data
openxlsx::write.xlsx(
  prewar_data_assigned_to_postwar,
  "Formatted Data/data_prewar_assigned_postwar.xlsx"
  )
