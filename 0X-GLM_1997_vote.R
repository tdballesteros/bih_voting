
### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# voting data
voting_data_1997 <- read.csv("Formatted Data/nationalist_votes_1997.csv")

# death data
death_data <- read.csv("Formatted Data/deaths_per_municipality_formatted.csv")

# municipality distance data
municipality_distances <- read.csv("Formatted Data/municipality_distances.csv")

# census data 1991
census_data_1991 <- read.csv("Formatted Data/census_data_1991_formatted.csv")

# municpality metadata
municipality_metadata <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")

# voting location 1997
voting_location_1997 <- read.csv("Formatted Data/voting_location_1997.csv")

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
  rbind(census_data_1991_2, census_data_1991_3)

#### canton/entity data ----------------------------------------------------------------------------
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
    rs_municipality = ifelse(entity == "Repuplika Srpska", 1, 0),
    fbih_municipality = ifelse(entity == "Federacija Bosna i Hercegovina", 1, 0)
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

### combine data -----------------------------------------------------------------------------------
df <- voting_data_1997 %>%
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  dplyr::full_join(canton_data, by = "municipality") %>%
  dplyr::full_join(death_data, by = "municipality") %>%
  dplyr::full_join(municipality_distances, by = "municipality") %>%
  dplyr::full_join(municipality_metadata, by = "municipality") %>%
  dplyr::full_join(voting_location_1997, by = "municipality") %>%
  dplyr::filter(!is.na(`Non.Nationalist`))

df_rs <- df %>%
  dplyr::filter(entity == "Repuplika Srpska")

df_fbih <- df %>%
  dplyr::filter(entity == "Federacija Bosna i Hercegovina")

### GLM: non-nationalist vote share ----------------------------------------------------------------
# A1 - Bosniak + Fractionalization
# A2 - Bosniak + Polarization
# A3 - Croat + Fractionalization
# A4 - Croat + Polarization
# A5 - Serb + Fractionalization
# A6 - Serb + Polarization
# A1r = A1 - Republika Serbska
# A1f = A1 - Federation
...

model_a1 <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                  percent_other + dist_hrv + dist_yug + dist_internal_border +
                  deaths_per_population + fractionalization + fbih_municipality + `split by line` +
                  `sarajevo district` + perc_cast_out,
                     data = df, family = gaussian())
summary(model_a1)
car::vif(model_a1)

model_a1r <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_rs, family = gaussian())
summary(model_a1r)
car::vif(model_a1r)

model_a1f <- glm(`Non.Nationalist` ~ log(total) + percent_muslims + percent_yugoslavs +
                   percent_other + dist_hrv + dist_yug + dist_internal_border +
                   deaths_per_population + fractionalization + `split by line` +
                   `sarajevo district` + perc_cast_out,
                 data = df_fbih, family = gaussian())
summary(model_a1f)
car::vif(model_a1f)

