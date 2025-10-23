
### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# voting data
voting_data_1997 <- read.csv("Formatted Data/nationalist_votes_1997.csv")

# death data
death_data <- read.csv("Formatted Data/deaths_per_postwar_municipality_formatted.csv")

# municipality distance data
municipality_distances <- read.csv("Formatted Data/municipality_distances.csv")

# census data 1991
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# census data 2013
census_data_2013 <- read.csv("Formatted Data/census_data_2013_formatted.csv")

# municpality metadata
municipality_metadata <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")

# voting location 1997
voting_location_1997 <- read.csv("Formatted Data/voting_location_1997.csv")

# load formatted shapefiles
source("01-Format_shapefiles.R")

### format data ------------------------------------------------------------------------------------
#### census data 1991 ------------------------------------------------------------------------------
census_data_1991 <- census_data_1991 %>%
  dplyr::mutate(
    # rename death data names to match with post-war standard naming
    municipality = dplyr::case_match(
      municipality,
      "Sarajevo Centar" ~	"Centar Sarajevo",
      "Bosanska Dubica" ~	"Dubica",
      "Gornji Vakuf" ~ "Gornji Vakuf-Uskoplje",
      "Kupres" ~ "Kupres-FBIH",
      "Bosanski Novi" ~ "Novi Grad",
      "Sarajevo Novi Grad" ~ "Novi Grad Sarajevo",
      "Sarajevo Novo Sarajevo" ~ "Novo Sarajevo",
      "Prozor" ~ "Prozor-Rama",
      "Bosanski Šamac" ~ "Šamac",
      "Skender Vakuf" ~ "Skender Vakuf / Kneževo",
      "Sarajevo Stari Grad" ~ "Stari Grad Sarajevo",
      "Trnovo" ~ "Trnovo-FBIH",
      "Sarajevo Vogošća" ~ "Vogošća",
      .default = municipality
    ))

# create additional rows for new post-war municipalities
census_data_1991_2 <- census_data_1991 %>%
  dplyr::mutate(municipality = dplyr::case_match(
    municipality,
    "Bosanska Krupa" ~	"Bužim",
    "Sanski Most" ~	"Oštra Luka",
    "Ključ" ~	"Ribnik",
    "Bosanski Petrovac" ~	"Petrovac",
    "Drvar" ~	"Istočni Drvar",
    "Kupres-FBIH" ~	"Kupres-RS",
    "Jajce" ~	"Jezero",
    "Skender Vakuf / Kneževo" ~	"Dobretići",
    "Doboj" ~	"Doboj Jug",
    "Gračanica" ~	"Petrovo",
    "Gradačac" ~	"Pelagićevo",
    "Odžak" ~	"Vukosavlje",
    "Šamac" ~	"Domaljevac-Šamac",
    "Orašje" ~	"Donji Žabar",
    "Lopare" ~	"Čelić",
    "Ugljevik" ~	"Teočak",
    "Zvornik" ~	"Sapna",
    "Kalesija" ~	"Osmaci",
    "Trebinje" ~	"Ravno",
    "Stolac" ~	"Berkovići",
    "Mostar" ~	"Istočni Mostar",
    "Trnovo-FBIH" ~	"Trnovo-RS",
    "Foča" ~	"Foča-Ustikolina",
    "Goražde" ~	"Novo Goražde",
    "Pale" ~	"Pale-Prača",
    "Ilidža" ~	"Istočna Ilidža",
    "Stari Grad Sarajevo" ~	"Istočni Stari Grad",
    "Novo Sarajevo" ~	"East New Sarajevo",
    "Novi Grad" ~	"Kostajnica",
    "Tešanj" ~	"Usora",
    "Vlasenica" ~	"Milići",
    .default = NA
  )) %>%
  dplyr::filter(!is.na(municipality))

census_data_1991_3 <- census_data_1991 %>%
  dplyr::mutate(municipality = dplyr::case_match(
    municipality,
    "Bosanska Krupa" ~ "Krupa na Uni",
    "Doboj" ~ "Doboj Istok",
    .default = NA
  )) %>%
  dplyr::filter(!is.na(municipality))

census_data_1991 <- census_data_1991 %>%
  rbind(census_data_1991_2, census_data_1991_3) %>%
  dplyr::select(municipality, total_1991 = total, percent_muslims_1991 = percent_muslims,
                percent_serbs_1991 = percent_serbs, percent_croats_1991 = percent_croats,
                percent_yugoslavs_1991 = percent_yugoslavs, percent_other_1991 = percent_other,
                fractionalization_1991 = fractionalization, polarization_1991 = polarization)

# remove unneeded dataframes from environment
rm(census_data_1991_2)
rm(census_data_1991_3)

#### census data 2013 ------------------------------------------------------------------------------
census_data_2013 <- census_data_2013 %>%
  dplyr::mutate(
    # rename death data names to match with post-war standard naming
    municipality = dplyr::case_match(
      municipality,
      "Kupres-FBiH" ~ "Kupres-FBIH",
      "Trnovo-FBiH" ~ "Trnovo-FBIH",
      "Doboj East" ~ "Doboj Istok",
      "Doboj South" ~ "Doboj Jug",
      "Bosanska Kostajnica" ~ "Kostajnica",
      .default = municipality
    )) %>%
  dplyr::select(municipality, total_2013 = population, percent_bosniaks_2013 = percent_bosniaks,
                percent_serbs_2013 = percent_serbs, percent_croats_2013 = percent_croats,
                percent_not_declared_2013 = percent_not_declared,
                percent_other_2013 = percent_other, fractionalization_2013 = fractionalization,
                polarization_2013 = polarization)

#### canton/entity data ----------------------------------------------------------------------------
# Brčko district is coded as part of both RS and FBIH.
canton_data <- bih_postwar_municipalities_shapefile_formatted %>%
  as.data.frame() %>%
  dplyr::select(municipality = NAME_3, entity = NAME_1, canton = NAME_2) %>%
  dplyr::mutate(
    municipality = dplyr::case_match(
      municipality,
      "Bosanska Kostajnica" ~ "Kostajnica",
      "Doboj East" ~ "Doboj Istok",
      "Doboj South" ~ "Doboj Jug",
      .default = municipality
    ),
    rs_municipality = ifelse(entity == "Repuplika Srpska" | municipality == "Brčko", 1, 0),
    fbih_municipality = ifelse(entity == "Federacija Bosna i Hercegovina"  | municipality == "Brčko", 1, 0)
  )

#### death data ------------------------------------------------------------------------------------
death_data <- death_data %>%
  dplyr::mutate(
    # rename death data names to match with post-war standard naming
    municipality = dplyr::case_match(
      municipality,
      "Sarajevo Centar" ~	"Centar Sarajevo", #x
      "Bosanska Dubica" ~	"Dubica",
      "Gornji Vakuf" ~ "Gornji Vakuf-Uskoplje",
      "Istočno Stari Grad" ~ "Istočni Stari Grad", #x
      "Kupres" ~ "Kupres-FBIH",
      "Bosanski Novi" ~ "Novi Grad",
      "Sarajevo Novi Grad" ~ "Novi Grad Sarajevo", #x
      "Sarajevo Novo Sarajevo" ~ "Novo Sarajevo",
      "Prozor" ~ "Prozor-Rama",
      "Bosanski Šamac" ~ "Šamac",
      "Skender Vakuf" ~ "Skender Vakuf / Kneževo",
      "Sarajevo Stari Grad" ~ "Stari Grad Sarajevo",
      "Trnovo" ~ "Trnovo-FBIH",
      "Sarajevo Vogošća" ~ "Vogošća",
      .default = municipality
    ),
    # recode ethnic dominance variable
    ethnic_dominance_cm = ifelse(ethnic_dominance_cm == "Yes", 1, 0)
    )

#### voting location 1997 --------------------------------------------------------------------------
voting_location_1997 <- voting_location_1997 %>%
  dplyr::mutate(
    # rename death data names to match with post-war standard naming
    municipality = dplyr::case_match(
      municipality,
      "Istočno Novo Sarajevo" ~ "East New Sarajevo",
      "Istočna Mostar" ~ "Istočni Mostar",
      "Kupres-FBiH" ~ "Kupres-FBIH",
      "Pale-RS" ~ "Pale",
      "Trnovo-FBiH" ~ "Trnovo-FBIH",
      # collapse portions of Mostar (FBIH) municipality
      "Mostar Jug" ~ "Mostar",
      "Mostar Jugoistok" ~ "Mostar",
      "Mostar Jugozapad" ~ "Mostar",
      "Mostar Sjever" ~ "Mostar",
      "Mostar Stari Grad" ~ "Mostar",
      "Mostar Zapad" ~ "Mostar",
      .default = municipality
      )) %>%
  # collapse Mostar entries into one entry
  dplyr::group_by(municipality) %>%
  dplyr::summarise(
    votes_cast_in_district = sum(votes_cast_in_district, na.rm = TRUE),
    votes_cast_out_district = sum(votes_cast_out_district, na.rm = TRUE),
    total_votes_cast = sum(total_votes_cast, na.rm = TRUE)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    perc_cast_in = votes_cast_in_district / total_votes_cast,
    perc_cast_out = votes_cast_out_district / total_votes_cast
  )

#### shapefile -------------------------------------------------------------------------------------
municipalities_data <- bih_postwar_municipalities_shapefile_formatted %>%
  as.data.frame() %>%
  dplyr::select(municipality = NAME_3, mun_area, mun_perimeter)

### combine data -----------------------------------------------------------------------------------
df <- voting_data_1997 %>%
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  dplyr::full_join(census_data_2013, by = "municipality") %>%
  dplyr::full_join(canton_data, by = "municipality") %>%
  dplyr::full_join(death_data, by = "municipality") %>%
  dplyr::full_join(municipality_distances, by = "municipality") %>%
  dplyr::full_join(municipality_metadata, by = "municipality") %>%
  dplyr::full_join(voting_location_1997, by = "municipality") %>%
  dplyr::full_join(municipalities_data, by = "municipality") %>%
  dplyr::filter(!is.na(`Non.Nationalist`)) %>%
  dplyr::mutate(
    delta_fractionalization = fractionalization_2013 - fractionalization_1991,
    delta_polarization = polarization_2013 - polarization_1991
  )

df_rs <- df %>%
  dplyr::filter(entity == "Repuplika Srpska")

df_fbih <- df %>%
  dplyr::filter(entity == "Federacija Bosna i Hercegovina")

### GLM: non-nationalist vote share ----------------------------------------------------------------
# A1 - Bosniak + Fractionalization
# A2 - Croat + Fractionalization
# A3 - Serb + Fractionalization
# A4 - Bosniak + Polarization
# A5 - Croat + Polarization
# A6 - Serb + Polarization
# A1r = A1 - Republika Serbska
# A1f = A1 - Federation
# ...

#### A1 models -------------------------------------------------------------------------------------

# country-wide
model_a1 <- glm(`Non.Nationalist` ~ log(total_1991) + percent_muslims_1991 +
                  percent_yugoslavs_1991 + percent_other_1991 + dist_hrv + dist_yug +
                  dist_internal_border + deaths_per_population + fractionalization_1991 +
                  delta_fractionalization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out + mun_area,
                     data = df, family = gaussian())
summary(model_a1)
car::vif(model_a1)

cor(df$fractionalization_1991, df$fractionalization_2013)

# republika srpska
model_a1r <- glm(`Non.Nationalist` ~ log(total_1991) + percent_muslims_1991 +
                   percent_yugoslavs_1991 + percent_other_1991 + dist_hrv + dist_yug +
                   dist_internal_border + deaths_per_population + fractionalization_1991 +
                   delta_fractionalization + `split by line` +
                   `sarajevo district` +
                   perc_cast_out + mun_area,
                 data = df_rs, family = gaussian())
summary(model_a1r)
car::vif(model_a1r)

# federation
model_a1f <- glm(`Non.Nationalist` ~ log(total_1991) + percent_muslims_1991 +
                   percent_yugoslavs_1991 + percent_other_1991 + dist_hrv + dist_yug +
                   dist_internal_border + deaths_per_population + fractionalization_1991 +
                   delta_fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out + mun_area,
                 data = df_fbih, family = gaussian())
summary(model_a1f)
car::vif(model_a1f)

#### A2 models -------------------------------------------------------------------------------------

# republic wide
model_a2 <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + fractionalization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                data = df, family = gaussian())
summary(model_a2)
car::vif(model_a2)

# republika srpska
model_a2r <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a2r)
car::vif(model_a2r)

# federation
model_a2f <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a2f)
car::vif(model_a2f)

#### A3 models -------------------------------------------------------------------------------------

# republic wide
model_a3 <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + fractionalization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                data = df, family = gaussian())
summary(model_a3)
car::vif(model_a3)

# republika srpska
model_a3r <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a3r)
car::vif(model_a3r)

# federation
model_a3f <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a3f)
car::vif(model_a3f)

#### A4 models -------------------------------------------------------------------------------------

# country wide
model_a4 <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + polarization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                data = df, family = gaussian())
summary(model_a4)
car::vif(model_a4)

# republika srpska
model_a4r <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a4r)
car::vif(model_a4r)

# federation
model_a4f <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a4f)
car::vif(model_a4f)

#### A5 models -------------------------------------------------------------------------------------

# country wide
model_a5 <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + polarization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                data = df, family = gaussian())
summary(model_a5)
car::vif(model_a5)

# republika srpska
model_a5r <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a5r)
car::vif(model_a5r)

# federation
model_a5f <- glm(`Non.Nationalist` ~ log(total) + percent_croats + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a5f)
car::vif(model_a5f)

#### A6 models -------------------------------------------------------------------------------------

# country wide
model_a6 <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + polarization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                data = df, family = gaussian())
summary(model_a6)
car::vif(model_a6)

# republika srpska
model_a6r <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a6r)
car::vif(model_a6r)

# federation
model_a6f <- glm(`Non.Nationalist` ~ log(total) + percent_serbs + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + polarization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a6f)
car::vif(model_a6f)

### GLM: nationalist vote share --------------------------------------------------------------------
# B1 - Bosniak + Fractionalization
# B2 - Croat + Fractionalization
# B3 - Serb + Fractionalization
# B4 - Bosniak + Polarization
# B5 - Croat + Polarization
# B6 - Serb + Polarization
# B1r = B1 - Republika Serbska
# B1f = B1 - Federation
# ...
