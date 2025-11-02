
### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(modelsummary)
library(tidyverse)

### load data --------------------------------------------------------------------------------------
# pre-war data
prewar_data <- readxl::read_xlsx("Formatted Data/data_prewar.xlsx")

# post-war data
postwar_data <- readxl::read_xlsx("Formatted Data/data_postwar.xlsx")

# pre-war data assigned to post-war municipalities
prewar_data_assigned_to_postwar <- readxl::read_xlsx(
  "Formatted Data/data_prewar_assigned_postwar.xlsx"
  )

### combine data -----------------------------------------------------------------------------------
df <- postwar_data %>%
  dplyr::full_join(prewar_data_assigned_to_postwar, by = "Municipality") %>%
  dplyr::filter(!is.na(`Proportion of Votes for Non-Nationalist Parties, 1997`)) %>%
  dplyr::mutate(
    `Population Density, 1991` = `Population, 1991` / `Municipality Area, Pre-War`,
    `Population, 1991 Logged` = log(`Population, 1991`),
    `Population, 2013 Logged` = log(`Population, 2013`),
    `Change in Ethnic Fractionalization, 1991 to 2013` = `Ethnic Fractionalization, 2013` -
      `Ethnic Fractionalization, 1991`,
    `Change in Ethnic Polarization, 1991 to 2013` = `Ethnic Polarization, 2013` -
      `Ethnic Polarization, 1991`,
    `Change in % Bosniaks, 1991 to 2013` = `Bosniak Population Percentage, 2013` -
      `Muslim Population Percentage, 1991`,
    `Change in % Croats, 1991 to 2013` = `Croat Population Percentage, 2013` -
      `Croat Population Percentage, 1991`,
    `Change in % Serbs, 1991 to 2013` = `Serb Population Percentage, 2013` -
      `Serb Population Percentage, 1991`,
    `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` = abs(`Change in % Bosniaks, 1991 to 2013`) +
      abs(`Change in % Croats, 1991 to 2013`) + abs(`Change in % Serbs, 1991 to 2013`),
    `Turnout Rate, 1997` = `Total Votes Cast, 1997` / `Population, 1991`,
    `Estimated Number of Minefields` = `Minefield Density` * `Municipality Area, Post-War`,
    `Municipality Area, Pre-War Logged` = log(`Municipality Area, Pre-War`),
    `Municipality Area, Post-War Logged` = log(`Municipality Area, Post-War`)
    )

df_rs <- df %>%
  dplyr::filter(`Republika Srpska Municipality` == 1)

df_fbih <- df %>%
  dplyr::filter(`Federation Municipality` == 1)

### GLM: non-nationalist vote share ----------------------------------------------------------------
# A1 - Bosniak + Fractionalization
# A2 - Croat + Fractionalization
# A3 - Serb + Fractionalization
### A4 - Bosniak + Polarization
### A5 - Croat + Polarization
### A6 - Serb + Polarization
# A1r = A1 - Republika Serbska
# A1f = A1 - Federation
# ...

#### A1 models -------------------------------------------------------------------------------------

# all municipalities
model_a1 <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Muslim Population Percentage, 1991` +
                  `Change in % Bosniaks, 1991 to 2013`,
                data = df, family = gaussian()
)
summary(model_a1)
car::vif(model_a1)

# Republika Srpska
model_a1r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Muslim Population Percentage, 1991` + `Change in % Bosniaks, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_a1r)
car::vif(model_a1r)

# Federation
model_a1f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Muslim Population Percentage, 1991` +
                   `Change in % Bosniaks, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_a1f)
car::vif(model_a1f)

#### A2 models -------------------------------------------------------------------------------------

# all municipalities
model_a2 <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Croat Population Percentage, 1991` +
                  `Change in % Croats, 1991 to 2013`,
                data = df, family = gaussian())
summary(model_a2)
car::vif(model_a2)

# Republika Srpska
model_a2r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Croat Population Percentage, 1991` + `Change in % Croats, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_a2r)
car::vif(model_a2r)

# Federation
model_a2f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Croat Population Percentage, 1991` +
                   `Change in % Croats, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_a2f)
car::vif(model_a2f)

#### A3 models -------------------------------------------------------------------------------------

# all municipalities
model_a3 <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Serb Population Percentage, 1991` +
                  `Change in % Serbs, 1991 to 2013`,
                data = df, family = gaussian())
summary(model_a3)
car::vif(model_a3)

# Republika Srpska
model_a3r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Serb Population Percentage, 1991` + `Change in % Serbs, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_a3r)
car::vif(model_a3r)

# Federation
model_a3f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Serb Population Percentage, 1991` +
                   `Change in % Serbs, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_a3f)
car::vif(model_a3f)

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

#### B1 models -------------------------------------------------------------------------------------

# country-wide
model_b1 <- glm(`Proportion of Votes for Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Muslim Population Percentage, 1991` +
                  `Change in % Bosniaks, 1991 to 2013`,
                data = df, family = gaussian())
summary(model_b1)
car::vif(model_b1)

# republika srpska
model_b1r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Muslim Population Percentage, 1991` + `Change in % Bosniaks, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_b1r)
car::vif(model_b1r)

# federation
model_b1f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Muslim Population Percentage, 1991` +
                   `Change in % Bosniaks, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_b1f)
car::vif(model_b1f)

#### B2 models -------------------------------------------------------------------------------------

# republic wide
model_b2 <- glm(`Proportion of Votes for Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Croat Population Percentage, 1991` +
                  `Change in % Croats, 1991 to 2013`,
                data = df, family = gaussian())
summary(model_b2)
car::vif(model_b2)

# republika srpska
model_b2r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Croat Population Percentage, 1991` + `Change in % Croats, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_b2r)
car::vif(model_b2r)

# federation
model_b2f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Croat Population Percentage, 1991` +
                   `Change in % Croats, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_b2f)
car::vif(model_b2f)

#### B3 models -------------------------------------------------------------------------------------

# republic wide
model_b3 <- glm(`Proportion of Votes for Nationalist Parties, 1997` ~
                  `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                  `Other Population Percentage, 1991` +
                  `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                  `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                  `Distance to IEBL, Post-War` + `Deaths per Population` +
                  `Ethnic Fractionalization, 1991` +
                  `Change in Ethnic Fractionalization, 1991 to 2013` + `Federation Municipality` +
                  `Sarajevo District` + `Proportion of Votes Cast Out-District, 1997` +
                  `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                  `Minefield Density` + `Serb Population Percentage, 1991` +
                  `Change in % Serbs, 1991 to 2013`,
                data = df, family = gaussian())
summary(model_b3)
car::vif(model_b3)

# republika srpska
model_b3r <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Serb Population Percentage, 1991` + `Change in % Serbs, 1991 to 2013`,
                 data = df_rs, family = gaussian())
summary(model_b3r)
car::vif(model_b3r)

# federation
model_b3f <- glm(`Proportion of Votes for Non-Nationalist Parties, 1997` ~
                   `Population, 1991 Logged` + `Yugoslav Population Percentage, 1991` +
                   `Other Population Percentage, 1991` +
                   `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` +
                   `Distance to Croatia, Post-War` + `Distance to Yugoslavia, Post-War` +
                   `Distance to IEBL, Post-War` + `Deaths per Population` +
                   `Ethnic Fractionalization, 1991` +
                   `Change in Ethnic Fractionalization, 1991 to 2013` + `Sarajevo District` +
                   `Proportion of Votes Cast Out-District, 1997` +
                   `Municipality Area, Post-War Logged` + `Turnout Rate, 1997` +
                   `Minefield Density` + `Croat Population Percentage, 1991` +
                   `Change in % Croats, 1991 to 2013`,
                 data = df_fbih, family = gaussian())
summary(model_b3f)
car::vif(model_b3f)

### model summaries --------------------------------------------------------------------------------

# A - Entire Country
modelsummary(list("A1" = model_a1, "A2" = model_a2, "A3" = model_a3), output = "markdown", stars = TRUE)
# A - Republika Srpska
modelsummary(list("A1-RS" = model_a1r, "A2-RS" = model_a2r, "A3-RS" = model_a3r), output = "markdown", stars = TRUE)
# A - Federation
modelsummary(list("A1-FBiH" = model_a1f, "A2-FBiH" = model_a2f, "A3-FBiH" = model_a3f), output = "markdown", stars = TRUE)

# B - Entire Country
modelsummary(list("B1" = model_b1, "B2" = model_b2, "B3" = model_b3), output = "markdown", stars = TRUE)
# B - Republika Srpska
modelsummary(list("B1-RS" = model_b1r, "B2-RS" = model_b2r, "B3-RS" = model_b3r), output = "markdown", stars = TRUE)
# B - Federation
modelsummary(list("B1-FBiH" = model_b1f, "B2-FBiH" = model_b2f, "B3-FBiH" = model_b3f), output = "markdown", stars = TRUE)


