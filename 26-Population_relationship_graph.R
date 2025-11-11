
### load libraries ---------------------------------------------------------------------------------
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

### load data ---------------------------------------------------------------------------------
# load models and model data
source("~/R/bih_voting/11-GLM_1997_vote.R", echo = FALSE)

### format data ---------------------------------------------------------------------------------
# create df with population values to predict, with means for all other variables held constant
population_effect_df <- data.frame(
  population = seq(
    from = min(df$`Population, 1991`, na.rm = TRUE), 
    to = max(df$`Population, 1991`, na.rm = TRUE), 
    length.out = 100
  )) %>%
  dplyr::rename(`Population, 1991` = population) %>%
  # calculate means of all variables
  dplyr::mutate(
    `Population, 1991 Logged` = log(`Population, 1991`),
    `Population, 1991 Redistributed Logged` = mean(df$`Population, 1991 Redistributed Logged`),
    `Yugoslav Population Percentage, 1991` = mean(df$`Yugoslav Population Percentage, 1991`),
    `Other Population Percentage, 1991` = mean(df$`Other Population Percentage, 1991`),
    `Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013` = mean(df$`Absolute Change in Bosniaks, Croats, and Serbs, 1991 to 2013`),
    `Distance to Croatia, Post-War` = mean(df$`Distance to Croatia, Post-War`),
    `Distance to Yugoslavia, Post-War` = mean(df$`Distance to Yugoslavia, Post-War`),
    `Distance to IEBL, Post-War` = mean(df$`Distance to IEBL, Post-War`),
    `Deaths per Population` = mean(df$`Deaths per Population`),
    `Ethnic Fractionalization, 1991` = mean(df$`Ethnic Fractionalization, 1991`),
    `Change in Ethnic Fractionalization, 1991 to 2013` = mean(df$`Change in Ethnic Fractionalization, 1991 to 2013`),
    `Republika Srpska Municipality` = mean(df$`Republika Srpska Municipality`),
    `Federation Municipality` = mean(df$`Federation Municipality`),
    `Sarajevo District` = mean(df$`Sarajevo District`),
    `Proportion of Votes Cast Out-District, 1997` = mean(df$`Proportion of Votes Cast Out-District, 1997`),
    `Municipality Area, Post-War Logged` = mean(df$`Municipality Area, Post-War Logged`),
    `Turnout Rate, 1997` = mean(df$`Turnout Rate, 1997`),
    `Minefield Density` = mean(df$`Minefield Density`),
    `Muslim Population Percentage, 1991` = mean(df$`Muslim Population Percentage, 1991`),
    `Change in % Bosniaks, 1991 to 2013` = mean(df$`Change in % Bosniaks, 1991 to 2013`),
    `Croat Population Percentage, 1991` = mean(df$`Croat Population Percentage, 1991`),
    `Change in % Croats, 1991 to 2013` = mean(df$`Change in % Croats, 1991 to 2013`),
    `Serb Population Percentage, 1991` = mean(df$`Serb Population Percentage, 1991`),
    `Change in % Serbs, 1991 to 2013` = mean(df$`Change in % Serbs, 1991 to 2013`),
    `Change in Population` = mean(df$`Change in Population`)
  )

# A1
prediction_a1 <- stats::predict.glm(
  model_a1, 
  newdata = population_effect_df, 
  type = "link",
  se.fit = TRUE
)

a1_fit <- prediction_a1$fit
a1_se_fit <- prediction_a1$se.fit

a1_fitted <- population_effect_df %>%
  dplyr::select(`Population, 1991`) %>%
  dplyr::mutate(
    fit = a1_fit,
    se_fit = a1_se_fit,
    lower_bound = fit - (1.96 * se_fit),
    upper_bound = fit + (1.96 * se_fit),
    Model = "A1"
  )

# A2
prediction_a2 <- stats::predict.glm(
  model_a2, 
  newdata = population_effect_df, 
  type = "link",
  se.fit = TRUE
)

a2_fit <- prediction_a2$fit
a2_se_fit <- prediction_a2$se.fit

a2_fitted <- population_effect_df %>%
  dplyr::select(`Population, 1991`) %>%
  dplyr::mutate(
    fit = a2_fit,
    se_fit = a2_se_fit,
    lower_bound = fit - (1.96 * se_fit),
    upper_bound = fit + (1.96 * se_fit),
    Model = "A2"
  )

# A3
prediction_a3 <- stats::predict.glm(
  model_a3,
  newdata = population_effect_df, 
  type = "link",
  se.fit = TRUE
)

a3_fit <- prediction_a3$fit
a3_se_fit <- prediction_a3$se.fit

a3_fitted <- population_effect_df %>%
  dplyr::select(`Population, 1991`) %>%
  dplyr::mutate(
    fit = a3_fit,
    se_fit = a3_se_fit,
    lower_bound = fit - (1.96 * se_fit),
    upper_bound = fit + (1.96 * se_fit),
    Model = "A3"
  )

# A2-FBiH
prediction_a2f <- stats::predict.glm(
  model_a2f, 
  newdata = population_effect_df, 
  type = "link",
  se.fit = TRUE
)

a2f_fit <- prediction_a2f$fit
a2f_se_fit <- prediction_a2f$se.fit

a2f_fitted <- population_effect_df %>%
  dplyr::select(`Population, 1991`) %>%
  dplyr::mutate(
    fit = a2f_fit,
    se_fit = a2f_se_fit,
    lower_bound = fit - (1.96 * se_fit),
    upper_bound = fit + (1.96 * se_fit),
    Model = "A2-FBiH"
  )

# B2
prediction_b2 <- stats::predict.glm(
  model_b2,
  newdata = population_effect_df, 
  type = "link",
  se.fit = TRUE
)

b2_fit <- prediction_b2$fit
b2_se_fit <- prediction_b2$se.fit

b2_fitted <- population_effect_df %>%
  dplyr::select(`Population, 1991`) %>%
  dplyr::mutate(
    fit = b2_fit,
    se_fit = b2_se_fit,
    lower_bound = fit - (1.96 * se_fit),
    upper_bound = fit + (1.96 * se_fit),
    Model = "B2"
  )


# pop_fitted <- rbind(a1_fitted, a2_fitted, a3_fitted)
pop_fitted <- rbind(a1_fitted, a2_fitted)

# plot
pop_relationship_graph_nonnationalist <- ggplot2::ggplot(data = pop_fitted) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = `Population, 1991`, ymin = lower_bound, ymax = upper_bound, group = Model, fill = Model), 
    alpha = 0.1
  ) +
  ggplot2::scale_fill_manual(values = c("#008000", "#ff0000", "#0047ab")) +
  ggplot2::geom_line(
    ggplot2::aes(x = `Population, 1991`, y = fit, group = Model, color = Model)
    ) +
  ggplot2::scale_color_manual(values = c("#008000", "#ff0000", "#0047ab")) +
  ggplot2::labs(
    x = "Pre-War Population",
    y = "Predicted Share of Non-Nationalist Votes"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2::scale_x_continuous(labels = scales::comma)


pop_fitted2 <- rbind(b2_fitted)

# plot
pop_relationship_graph_nationalist <- ggplot2::ggplot(data = pop_fitted2) +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = `Population, 1991`, ymin = lower_bound, ymax = upper_bound, group = Model, fill = Model), 
    alpha = 0.1
  ) +
  ggplot2::scale_fill_manual(values = c("#ff0000")) +
  ggplot2::geom_line(
    ggplot2::aes(x = `Population, 1991`, y = fit, group = Model, color = Model)
  ) +
  ggplot2::scale_color_manual(values = c("#ff0000")) +
  ggplot2::labs(
    x = "Pre-War Population",
    y = "Predicted Share of Nationalist Votes"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  ggplot2::scale_x_continuous(labels = scales::comma)


