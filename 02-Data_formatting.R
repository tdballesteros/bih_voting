
# This script formats the raw datafiles and exports the formatted data.

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(tidyverse)

### Pre-War Data -----------------------------------------------------------------------------------
# This section formats data that is from before the 1992-1995 war or is based upon the pre-war
# municipalities.

#### 1991 Census Data ------------------------------------------------------------------------------
# Note: The total row's Total value shows a population 1,402 individuals greater than the sum of the
# individual ethnic groups. All municipality-level Total values match the sum of the breakdown
# values.
census_data_1991 <- readxl::read_xlsx("Data/bih_census_1991.xlsx") %>%
  # drop Bosnia-wide row
  dplyr::filter(Municipality != "Total") %>%
  # rename Muslims (Bosniaks) variable
  dplyr::rename(muslims = `Muslims by nationality`) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(
    # calculate municipality-level percentages by ethnicity
    percent_muslims = muslims / total,
    percent_serbs = serbs / total,
    percent_croats = croats / total,
    percent_yugoslavs = yugoslavs / total,
    percent_other = other / total,
    # calculate polarization and fractionalization metrics
    fractionalization = 1 - (percent_muslims^2 + percent_serbs^2 + percent_croats^2 +
                               percent_yugoslavs^2 + percent_other^2),
    polarization = 1 - 4 * ((percent_muslims * (0.5 - percent_muslims)^2) +
                              (percent_serbs * (0.5 - percent_serbs)^2) +
                              (percent_croats * (0.5 - percent_croats)^2) +
                              (percent_yugoslavs * (0.5 - percent_yugoslavs)^2) +
                              (percent_other * (0.5 - percent_other)^2)),
    percent_total_population = total / sum(total),
    # rename pre-war municipalities to standardize names
    municipality = dplyr::case_match(
      municipality,
      "Centar Sarajevo" ~ "Sarajevo Centar",
      "Dubica" ~ "Bosanska Dubica",
      "Gradiška" ~ "Bosanska Gradiška",
      "Novi Grad" ~ "Bosanski Novi",
      "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
      "Novo Sarajevo" ~ "Sarajevo Novo Sarajevo",
      "Šamac" ~ "Bosanski Šamac",
      "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
      "Vogošća" ~ "Sarajevo Vogošća",
      .default = municipality
    )
  )

# write formatted data
write.csv(census_data_1991, "Formatted Data/census_data_1991_formatted.csv", row.names = FALSE)

#### 1991 Census Data by Community -----------------------------------------------------------------
census_data_1991_community <- readxl::read_xlsx(
  "Data/Stanovništvo prema nacionalnom izjašnjavanju po mjesnim zajednicama 1991.xlsx"
  ) %>%
  dplyr::filter(
    # drop opština and country total rows
    `Mjesne Zajednice` != "TOTAL",
    # drop country total percentages row
    !is.na(`Mjesne Zajednice`)
  ) %>%
  dplyr::select(municipality = Opština, community = `Mjesne Zajednice`, total = Ukupno,
                muslims = Muslimani, serbs = Srbi, croats = Hrvati, yugoslavs = Jugosloveni,
                others = Ostali) %>%
  dplyr::mutate(
    muslims_percent = muslims / total,
    serbs_percent = serbs / total,
    croats_percent = croats / total,
    yugoslavs_percent = yugoslavs / total,
    others_percent = others / total
  ) %>%
  dplyr::group_by(municipality) %>%
  dplyr::mutate(population_percent_of_municipality = total / sum(total)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # calculate polarization and fractionalization metrics
    fractionalization = 1 - (muslims_percent^2 + serbs_percent^2 + croats_percent^2 +
                               yugoslavs_percent^2 + others_percent^2),
    polarization = 1 - 4 * ((muslims_percent * (0.5 - muslims_percent)^2) +
                              (serbs_percent * (0.5 - serbs_percent)^2) +
                              (croats_percent * (0.5 - croats_percent)^2) +
                              (yugoslavs_percent * (0.5 - yugoslavs_percent)^2) +
                              (others_percent * (0.5 - others_percent)^2))
  ) %>%
  dplyr::arrange(municipality, community)

# write formatted data
write.csv(
  census_data_1991_community,
  "Formatted Data/census_data_1991_community_formatted.csv",
  row.names = FALSE
  )

#### Death Data ------------------------------------------------------------------------------------
death_data_original <- readxl::read_xlsx("Data/deaths_per_municipality.xlsx") %>%
  # rename variables
  dplyr::select(municipality = Municipality, fractionalization_cm = Fractionalization,
                polarization_cm = Polarization, ethnic_dominance_cm = `Ethnic Dominance`,
                deaths_per_population = `Deaths/Population`) %>%
  dplyr::mutate(
    # rename pre-war municipalities to standardize names
    municipality = ifelse(municipality == "Sarajevo Novo Sar.",
                          "Sarajevo Novo Sarajevo",
                          municipality)
    ) %>%
  # merge in 1991 census data to calculate absolute death values
  dplyr::full_join(census_data_1991, by = "municipality") %>%
  # calculate approximate total death counts based on 1991 census population values
  dplyr::mutate(deaths_count = total * deaths_per_population) %>%
  # drop unneeded population variables
  dplyr::select(municipality, deaths_per_population, deaths_count, fractionalization_cm,
                polarization_cm, ethnic_dominance_cm) %>%
  dplyr::arrange(municipality)

# # create additional rows for new post-war municipalities
# death_data2 <- death_data_original %>%
#   dplyr::mutate(municipality = dplyr::case_match(
#     municipality,
#     "Bosanska Krupa" ~	"Bužim",
#     "Sanski Most" ~	"Oštra Luka",
#     "Ključ" ~	"Ribnik",
#     "Bosanski Petrovac" ~	"Petrovac",
#     "Drvar" ~	"Istočni Drvar",
#     "Kupres" ~	"Kupres-RS",
#     "Jajce" ~	"Jezero",
#     "Kneževo" ~	"Dobretići",
#     "Doboj" ~	"Doboj Jug",
#     "Gračanica" ~	"Petrovo",
#     "Gradačac" ~	"Pelagićevo",
#     "Odžak" ~	"Vukosavlje",
#     "Šamac" ~	"Domaljevac-Šamac",
#     "Orašje" ~	"Donji Žabar",
#     "Lopare" ~	"Čelić",
#     "Ugljevik" ~	"Teočak",
#     "Zvornik" ~	"Sapna",
#     "Kalesija" ~	"Osmaci",
#     "Trebinje" ~	"Ravno",
#     "Stolac" ~	"Berkovići",
#     "Mostar" ~	"Istočni Mostar",
#     "Trnovo" ~	"Trnovo-RS",
#     "Foča" ~	"Foča-Ustikolina",
#     "Goražde" ~	"Novo Goražde",
#     "Pale" ~	"Pale-Prača",
#     "Ilidža" ~	"Istočna Ilidža",
#     "Sarajevo Stari Grad" ~	"Istočno Stari Grad",
#     "Sarajevo Novo Sarajevo" ~	"East New Sarajevo",
#     "Bosanski Novi" ~	"Kostajnica",
#     "Tešanj" ~	"Usora",
#     "Vlasenica" ~	"Milići",
#     .default = NA
#   )) %>%
#   dplyr::filter(!is.na(municipality))
# 
# death_data3 <- death_data_original %>%
#   dplyr::mutate(municipality = dplyr::case_match(
#     municipality,
#     "Bosanska Krupa" ~ "Krupa na Uni",
#     "Doboj" ~ "Doboj Istok",
#     .default = NA
#   )) %>%
#   dplyr::filter(!is.na(municipality))
# 
# death_data_postwar_municipalities <- death_data_original %>%
#   rbind(death_data2, death_data3)

# write formatted data
write.csv(death_data_original, "Formatted Data/deaths_per_municipality_formatted.csv", row.names = FALSE)
# write.csv(death_data_postwar_municipalities, "Formatted Data/deaths_per_postwar_municipality_formatted.csv", row.names = FALSE)



### Post-War Data ----------------------------------------------------------------------------------
# This section formats data that is from after the 1992-1995 war or is based upon the post-war
# municipalities.

#### 2013 Census Data ------------------------------------------------------------------------------
census_data_2013 <- readxl::read_xlsx("Data/bih_census_2013.xlsx") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(
    # calculate municipality-level percentages by ethnicity
    total = bosniak + croat + serb + `not declared` + other + `no answer`,
    percent_bosniaks = bosniak / total,
    percent_serbs = serb / total,
    percent_croats = croat / total,
    percent_not_declared = `not declared` / total,
    percent_other = other / total,
    percent_no_answer = `no answer` / total,
    # calculate polarization and fractionalization metrics
    fractionalization = 1 - (percent_bosniaks^2 + percent_serbs^2 + percent_croats^2 +
                               percent_not_declared^2 + percent_other^2 + percent_no_answer^2),
    polarization = 1 - 4 * ((percent_bosniaks * (0.5 - percent_bosniaks)^2) +
                              (percent_serbs * (0.5 - percent_serbs)^2) +
                              (percent_croats * (0.5 - percent_croats)^2) +
                              (percent_not_declared * (0.5 - percent_not_declared)^2) +
                              (percent_other * (0.5 - percent_other)^2) +
                              (percent_no_answer * (0.5 - percent_no_answer)^2)),
    percent_total_population = total / sum(total),
    # rename post-war municipalities to standardize names
    municipality = dplyr::case_match(
      municipality,
      "Centar Sarajevo" ~ "Sarajevo Centar",
      "Doboj - Istok" ~ "Doboj Istok",
      "Doboj - Jug" ~ "Doboj Jug",
      "Foča-FBiH" ~ "Foča-Ustikolina",
      "Foča-RS" ~ "Foča",
      "Gornji Vakuf - Uskoplje" ~ "Gornji Vakuf-Uskoplje",
      "Kneževo" ~ "Skender Vakuf",
      "Kozarska Dubica" ~ "Dubica",
      "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
      "Pale-FBiH" ~ "Pale-Prača",
      "Pale-RS" ~ "Pale",
      "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
      # "Brod" ~ "Bosanski Brod",
      # "Doboj - Istok" ~	"Doboj East",
      # "Doboj - Jug" ~	"Doboj South",
      # "Foča-FBiH" ~	"Foča-Ustikolina",
      # "Foča-RS" ~	"Foča",
      # "Gornji Vakuf - Uskoplje" ~	"Gornji Vakuf-Uskoplje",
      # "Gradiška" ~ "Bosanska Gradiška",
      # "Istočno Novo Sarajevo" ~	"East New Sarajevo",
      # "Kneževo" ~	"Skender Vakuf / Kneževo",
      # "Kostajnica" ~ "Bosanska Kostajnica",
      # "Kozarska Dubica" ~	"Dubica",
      # "Pale-FBiH" ~	"Pale-Prača",
      # "Pale-RS" ~	"Pale",
      .default = municipality
    )) %>%
  dplyr::arrange(municipality)

# write formatted data
write.csv(census_data_2013, "Formatted Data/census_data_2013_formatted.csv", row.names = FALSE)

#### 1997 Municipal Election Results Tally ---------------------------------------------------------
election_tally <- readxl::read_xlsx("Data/bih_municipal_votes_97_tally.xlsx") %>%
  # rename variables
  dplyr::select(municipality_code = MunCode, municipality = Municipality, party = `Party/Coalition`,
                vote_count = Votes, total_municipal_votes = TotMunVotes) %>%
  dplyr::mutate(
    # rename post-war municipalities to standardize names
    municipality = dplyr::case_match(
      municipality,
      "Bosanska Dubica / Kozarska Dubica" ~ "Dubica",
      "Bosanska Gradiška / Gradiška" ~ "Gradiška",
      "Bosanska Krupa / Krupa na Uni" ~ "Krupa na Uni",
      "Bosanski Brod / Srpski Brod" ~ "Brod",
      "Bosanski Novi / Novi Grad" ~ "Novi Grad",
      "Bosanski Petrovac / Petrovac" ~ "Petrovac",
      "Bosanski Šamac / Šamac" ~ "Šamac",
      "Bosansko Grahovo / Grahovo" ~ "Bosansko Grahovo",
      "Centar Sarajevo" ~ "Sarajevo Centar",
      "Drvar / Srpski Drvar" ~ "Istočni Drvar",
      "Foča / Srbinje" ~ "Foča",
      "Goražde / Srpsko Goražde" ~ "Novo Goražde",
      "Gornji Vakuf" ~ "Gornji Vakuf-Uskoplje",
      "Gračanica / Petrovo" ~ "Petrovo",
      "Gradačac / Pelagićevo" ~ "Pelagićevo",
      "Ilidža / Srpska Ilidža" ~ "Istočna Ilidža",
      "Jajce / Jezero" ~ "Jezero",
      "Kalesija / Osmaci" ~ "Osmaci",
      "Ključ / Ribnik" ~ "Ribnik",
      "Kupres" ~ "Kupres-FBIH",
      "Kupres / Srpski Kupres" ~ "Kupres-RS",
      "Mostar / Srpski Mostar" ~ "Istočni Mostar",
      "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
      "Novo Sarajevo / Srpsko Novo Sarajevo" ~ "Istočno Novo Sarajevo",
      "Odžak / Vukosavlje" ~ "Vukosavlje",
      "Orašje / Srpsko Orašje" ~ "Donji Žabar",
      "Pale (RS)" ~ "Pale",
      "Prozor / Prozor-Rama" ~ "Prozor-Rama",
      "Sanski Most / Srpski Sanski Most" ~ "Oštra Luka",
      "Skender Vakuf / Kneževo" ~ "Skender Vakuf",
      "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
      "Stari Grad Sarajevo / Srpski Stari Grad" ~ "Istočni Stari Grad",
      "Stolac / Berkovići" ~ "Berkovići",
      "Trnovo (FBIH)" ~ "Trnovo-FBIH",
      "Trnovo (RS)" ~ "Trnovo-RS",
      # "Stolac / Berkovići" ~ "Berkovići",
      # "Bosansko Grahovo / Grahovo" ~ "Bosansko Grahovo",
      # "Bosanski Brod / Srpski Brod" ~ "Bosanski Brod",
      # "Orašje / Srpsko Orašje" ~ "Donji Žabar",
      # "Foča / Srbinje" ~ "Foča",
      # "Gornji Vakuf" ~ "Gornji Vakuf-Uskoplje",
      # "Bosanska Gradiška / Gradiška" ~ "Bosanska Gradiška",
      # "Ilidža / Srpska Ilidža" ~ "Istočna Ilidža",
      # "Drvar / Srpski Drvar" ~ "Istočni Drvar",
      # "Mostar / Srpski Mostar" ~ "Istočni Mostar",
      # "Stari Grad Sarajevo / Srpski Stari Grad" ~ "Istočni Stari Grad",
      # "Novo Sarajevo / Srpsko Novo Sarajevo" ~ "East New Sarajevo",
      # "Jajce / Jezero" ~ "Jezero",
      # "Bosanska Dubica / Kozarska Dubica" ~ "Dubica",
      # "Bosanska Krupa / Krupa na Uni" ~ "Krupa na Uni",
      # "Kupres" ~ "Kupres-FBIH",
      # "Kupres / Srpski Kupres" ~ "Kupres-RS",
      # "Bosanski Novi / Novi Grad" ~ "Novi Grad",
      # "Goražde / Srpsko Goražde" ~ "Novo Goražde",
      # "Kalesija / Osmaci" ~ "Osmaci",
      # "Sanski Most / Srpski Sanski Most" ~ "Oštra Luka",
      # "Pale (RS)" ~ "Pale",
      # "Gradačac / Pelagićevo" ~ "Pelagićevo",
      # "Bosanski Petrovac / Petrovac" ~ "Petrovac",
      # "Gračanica / Petrovo" ~ "Petrovo",
      # "Prozor / Prozor-Rama" ~ "Prozor-Rama",
      # "Ključ / Ribnik" ~ "Ribnik",
      # "Bosanski Šamac / Šamac" ~ "Šamac",
      # "Trnovo (FBIH)" ~ "Trnovo-FBIH",
      # "Trnovo (RS)" ~ "Trnovo-RS",
      # "Odžak / Vukosavlje" ~ "Vukosavlje",
      # collapse portions of Mostar (FBIH) municipality
      "Mostar Jug" ~ "Mostar",
      "Mostar Jugoistok" ~ "Mostar",
      "Mostar Jugozapad" ~ "Mostar",
      "Mostar Sjever" ~ "Mostar",
      "Mostar Stari Grad" ~ "Mostar",
      "Mostar Zapad" ~ "Mostar",
      .default = municipality
    )) %>%
  # collapse results by municipality and party to summarise Mostar
  dplyr::group_by(municipality, party) %>%
  dplyr::summarise(
    municipality_code = max(municipality_code),
    vote_count = sum(vote_count),
    total_municipal_votes = sum(total_municipal_votes)
  ) %>%
  dplyr::ungroup() %>%
  # fix Mostar total municipal votes values
  dplyr::group_by(municipality) %>%
  dplyr::mutate(
    total_municipal_votes = max(total_municipal_votes)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # create new municipality code for merged Mostar municipalities
    municipality_code = ifelse(municipality == "Mostar", "151", municipality_code),
    # calculate percentage of vote each party received per municipality
    percent_vote = vote_count / total_municipal_votes
    ) %>%
  dplyr::arrange(municipality)

# write formatted data
write.csv(election_tally, "Formatted Data/election_tally_1997_formatted.csv", row.names = FALSE)

# post-war municipalities without data:
## Doboj Istok, Doboj Jug, Dobretići, Domaljevac-Šamac, Foča-FBiH, Kostajnica, Milići, Pale-FBiH,
## Ravno, Sapna, Teočak, Usora
    
#### 1997 Municipal Election Results Seat Allocation -----------------------------------------------
seat_tally <- read.csv("Data/bih_municipal_elections_97_seat_allocation.csv") %>%
  dplyr::rename(
    municipality = Municipality,
    canton = Canton,
    entity = Entity,
    voters = Voters,
    `SRS*` = `SRS....`
  ) %>%
  dplyr::mutate(
    # rename post-war municipalities to standardize names
    municipality = dplyr::case_match(
      municipality,
      "Banovici" ~ "Banovići",
      "Bihac" ~ "Bihać",
      "Bileca" ~ "Bileća",
      "Bosanska Dubica" ~ "Dubica",
      "Bosanska Gradiska" ~ "Gradiška",
      "Bosanska Krupa RS" ~ "Krupa na Uni",
      "Bosanski Brod" ~ "Brod",
      "Bosanski Novi" ~ "Novi Grad",
      "Bosanski Petrovac RS" ~ "Petrovac",
      "Bosanski Samac" ~ "Šamac",
      "Brcko" ~ "Brčko",
      "Busovaca" ~ "Busovača",
      "Buzim" ~ "Bužim",
      "Cajnice" ~ "Čajniče",
      "Caplijna" ~ "Čapljina",
      "Celinac" ~ "Čelinac",
      "Centar Sarajevo" ~ "Sarajevo Centar",
      "Citluk" ~ "Čitluk",
      "Drvar RS" ~ "Istočni Drvar",
      "Foca" ~ "Foča",
      "Glamoc" ~ "Glamoč",
      "Gorazde" ~ "Goražde",
      "Gorazde RS" ~ "Novo Goražde",
      "Gornji Vakuf" ~ "Gornji Vakuf-Uskoplje",
      "Gracanica" ~ "Gračanica",
      "Gracanica RS" ~ "Petrovo",
      "Gradacac" ~ "Gradačac",
      "Gradacac RS" ~ "Pelagićevo",
      "Hadzici" ~ "Hadžići",
      "Ilidza" ~ "Ilidža",
      "Ilidza RS" ~ "Istočna Ilidža",
      "Ilijas" ~ "Ilijaš",
      "Jajce RS" ~ "Jezero",
      "Kalesija RS" ~ "Osmaci",
      "Kljuc" ~ "Ključ",
      "Kljuc RS" ~ "Ribnik",
      "Kotor Varos" ~ "Kotor Varoš",
      "Kresevo" ~ "Kreševo",
      "Kupres" ~ "Kupres-FBIH",
      "Kupres RS" ~ "Kupres-RS",
      "Laktasi" ~ "Laktaši",
      "Ljubuski" ~ "Ljubuški",
      "Lopare" ~ "Čelić",
      "Lopare RS" ~ "Lopare",
      "Modrica" ~ "Modriča",
      "Mostar RS" ~ "Istočni Mostar",
      "Mrkonjic Grad" ~ "Mrkonjić Grad",
      "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
      "Novo Sarajevo RS" ~ "Istočno Novo Sarajevo",
      "Odzak" ~ "Odžak",
      "Odzak RS" ~ "Vukosavlje",
      "Orasje" ~ "Orašje",
      "Orasje RS" ~ "Donji Žabar",
      "Posusje" ~ "Posušje",
      "Prozor" ~ "Prozor-Rama",
      "Sanski Most RS" ~ "Oštra Luka",
      "Sekovici" ~ "Šekovići",
      "Sipovo" ~ "Šipovo",
      "Siroki Brijeg" ~ "Široki Brijeg",
      "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
      "Stari Grad Sarajevo RS" ~ "Istočni Stari Grad",
      "Stolac RS" ~ "Berkovići",
      "Tesanj" ~ "Tešanj",
      "Teslic" ~ "Teslić",
      "Trnovo" ~ "Trnovo-FBIH",
      "Trnovo RS" ~ "Trnovo-RS",
      "Vares" ~ "Vareš",
      "Velika Kladusa" ~ "Velika Kladuša",
      "Visegrad" ~ "Višegrad",
      "Vogosca" ~ "Vogošća",
      "Zavidovici" ~ "Zavidovići",
      "Zepce" ~ "Žepče",
      "Zivinice" ~ "Živinice",
      # "Banovici" ~ "Banovići",
      # "Bihac" ~ "Bihać",
      # "Bileca" ~ "Bileća",
      # "Bosanska Dubica" ~ "Kozarska Dubica",
      # "Bosanska Gradiska" ~ "Gradiška",
      # "Bosanska Krupa RS" ~ "Krupa na Uni",
      # "Bosanski Brod" ~ "Brod",
      # "Bosanski Novi" ~ "Novi Grad",
      # "Bosanski Petrovac RS" ~ "Petrovac",
      # "Bosanski Samac" ~ "Šamac",
      # "Busovaca" ~ "Busovača",
      # "Brcko" ~ "Brčko",
      # "Buzim" ~ "Bužim",
      # "Cajnice" ~ "Čajniče",
      # "Capljina" ~ "Čapljina",
      # "Celinac" ~ "Čelinac",
      # "Citluk" ~ "Čitluk",
      # "Drvar RS" ~ "Istočni Drvar",
      # "Foca" ~ "Foča-RS",
      # "Glamoc" ~ "Glamoč",
      # "Gorazde" ~ "Goražde",
      # "Gorazde RS" ~ "Novo Goražde",
      # "Gornji Vakuf" ~ "Gornji Vakuf - Uskoplje",
      # "Gracanica" ~ "Gračanica",
      # "Gracanica RS" ~ "Petrovo",
      # "Gradacac" ~ "Gradačac",
      # "Gradacac RS" ~ "Pelagićevo",
      # "Hadzici" ~ "Hadžići",
      # "Ilidza" ~ "Ilidža",
      # "Ilidza Rs" ~ "Istočna Ilidža",
      # "Ilijas" ~ "Ilijaš",
      # "Jajce RS" ~ "Jezero",
      # "Kalesija RS" ~ "Osmaci",
      # "Kljuc" ~ "Ključ",
      # "Kljuc" ~ "Ribnik",
      # "Kotor Varos" ~ "Kotor Varoš",
      # "Kresevo" ~ "Kreševo",
      # "Kupres" ~ "Kupres-FBiH",
      # "Kupres RS" ~ "Kupres-RS",
      # "Laktasi" ~ "Laktaši",
      # "Ljubuski" ~ "Ljubuški",
      # "Lopare RS" ~ "Čelić",
      # "Modrica" ~ "Modriča",
      # "Mostar RS" ~ "Istočni Mostar",
      # "Mrkonjić Grad" ~ "Mrkonjić Grad",
      # "Novo Sarajevo RS" ~ "Istočno Novo Sarajevo",
      # "Odzak" ~ "Odžak",
      # "Odzak RS" ~ "Vukosavlje",
      # "Orasje" ~ "Orašje",
      # "Orasje RS" ~ "Donji Žabar",
      # "Pale" ~ "Pale-RS",
      # "Posusje" ~ "Posušje",
      # "Prozor" ~ "Prozor-Rama",
      # "Sanski Most RS" ~ "Oštra Luka",
      # "Sekovici" ~ "Šekovići",
      # "Sipovo" ~ "Šipovo",
      # "Siroki Brijeg" ~ "Široki Brijeg",
      # "Skender Vakuf" ~ "Kneževo",
      # "Stari Grad Sarajevo RS" ~ "Istočni Stari Grad",
      # "Berkovici" ~ "Berkovići",
      # "Tesanj" ~ "Tešanj",
      # "Teslic" ~ "Teslić",
      # "Trnovo" ~ "Trnovo-FBiH",
      # "Trnovo RS" ~ "Trnovo-RS",
      # "Vares" ~ "Vareš",
      # "Velika Kladusa" ~ "Velika Kladuša",
      # "Visegrad" ~ "Višegrad",
      # "Vogosca" ~ "Vogošća",
      # "Zavidovici" ~ "Zavidovići",
      # "Zepce" ~ "Žepče",
      # "Zivinice" ~ "Živinice",
      # collapse portions of Mostar (FBIH) municipality
      "Mostar Jug" ~ "Mostar",
      "Mostar Jugoistok" ~ "Mostar",
      "Mostar Jugozapad" ~ "Mostar",
      "Mostar Sjever" ~ "Mostar",
      "Mostar Stari Grad" ~ "Mostar",
      "Mostar Zapad" ~ "Mostar",
      .default = municipality
    )) %>%
  dplyr::arrange(municipality)

# write formatted data
write.csv(seat_tally, "Formatted Data/seat_tally_1997_formatted.csv", row.names = FALSE)

# post-war municipalities without data:
## Doboj Istok, Doboj Jug, Dobretići, Domaljevac-Šamac, Foča-FBiH, Kostajnica, Milići, Pale-FBiH,
## Ravno, Sapna, Teočak, Usora

#### Nationalist Votes 1997 -------------------------------------------------------------------------
nat_votes_1997 <- readxl::read_xlsx("Data/bih_parties_1997_leanings.xlsx") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::full_join(election_tally, by = "party") %>%
  dplyr::filter(
    !is.na(municipality),
    !municipality %in%
      c("Lista na Nivou / Razini Grada Mostara za Gradsko Vijeće",
        "Općina Mostar Jug za Gradsko Vijeće Grada Mostara",
        "Općina Mostar Jugoistok za Gradsko Vijeće Grada Mostara",
        "Općina Mostar Jugozapad za Gradsko Vijeće Grada Mostara",
        "Općina Mostar Sjever za Gradsko Vijeće Grada Mostara",
        "Općina Mostar Zapad za Gradsko Vijeće Grada Mostara")
      ) %>%
  # collapse by binary value
  dplyr::group_by(municipality, binary) %>%
  dplyr::summarise(
    # vote_count = sum(vote_count, na.rm = TRUE),
    # total_municipal_votes = max(total_municipal_votes, na.rm = TRUE),
    percent_vote = sum(percent_vote, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = binary, values_from = percent_vote) %>%
  replace(is.na(.), 0) %>%
  dplyr::arrange(municipality)

# write formatted data
write.csv(nat_votes_1997, "Formatted Data/nationalist_votes_1997.csv", row.names = FALSE)

#### Voting Location 1997 --------------------------------------------------------------------------
voting_location_1997 <- readxl::read_xlsx("Data/bih_voting_location_1997.xlsx") %>%
  dplyr::select(municipality = Municipality, votes_cast_in_district = NumInDist,
                votes_cast_out_district = NumOutDist, total_votes_cast = TotalVotes) %>%
  dplyr::mutate(
    perc_cast_in = votes_cast_in_district / total_votes_cast,
    perc_cast_out = votes_cast_out_district / total_votes_cast,
    # rename post-war municipalities to standardize names
    municipality = dplyr::case_match(
      municipality,
      "Bosanska Gradiška" ~ "Gradiška",
      "Bosanski Brod" ~ "Brod",
      "Centar Sarajevo" ~ "Sarajevo Centar",
      "Doboj-Istok" ~ "Doboj Istok",
      "Doboj-Jug" ~ "Doboj Jug",
      "Foča-FBiH" ~ "Foča-Ustikolina",
      "Istočna Mostar" ~ "Istočni Mostar",
      "Novi Grad Sarajevo" ~ "Sarajevo Novi Grad",
      "Pale-FBiH" ~ "Pale-Prača",
      "Pale-RS" ~ "Pale",
      "Skender Vakuf / Kneževo" ~ "Skender Vakuf",
      "Stari Grad Sarajevo" ~ "Sarajevo Stari Grad",
      .default = municipality
    )
  ) %>%
  dplyr::arrange(municipality)

# write formatted data
write.csv(voting_location_1997, "Formatted Data/voting_location_1997.csv", row.names = FALSE)

