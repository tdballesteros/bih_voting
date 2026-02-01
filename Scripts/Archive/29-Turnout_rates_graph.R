
library(ggplot2)
library(ggbeeswarm)
library(stringr)
library(dplyr)

turnout <- df %>%
  dplyr::select(Municipality, Entity, `Turnout Rate, 1997`) %>%
  dplyr::mutate(
    Municipality = stringr::str_replace_all(Municipality, "[[:space:]]", " "),
    Entity = stringr::str_replace_all(Entity, "[[:space:]]", " "),
    Entity = ifelse(Entity == "Federacija Bosna i Hercegovina", "Federation", Entity))

# pull Brčko value
turnout_brcko <- turnout$`Turnout Rate, 1997`[turnout$Municipality == "Brčko"]
  
# append Brčko as both RS and FBiH
turnout <- turnout %>%
  rbind(tibble(
    Municipality = c("Brčko", "Brčko"),
    Entity = c("Republika Srpska", "Federation"),
    `Turnout Rate, 1997` = c(turnout_brcko, turnout_brcko)
  )) %>%
  # drop Brčko as an Entity
  dplyr::filter(Entity != "Brčko")

turnout_graph <- ggplot2::ggplot(
  turnout,
  ggplot2::aes(
    x = `Turnout Rate, 1997`,
    y = Entity,
    fill = `Turnout Rate, 1997`,
    text = paste0(
      "Municipality: ", Municipality, "\n", 
      "Turnout Rate: ", round(100 * `Turnout Rate, 1997`, 1), "%"
    ))
  ) +
  ggbeeswarm::geom_beeswarm(
    shape = 21,
    color = "black",
    size = 3,
    cex = 3
    ) +
  ggplot2::scale_fill_gradient(
    low = "gray90",
    high = "black",
    name = "Turnout Rate",
    labels = scales::label_percent()
    ) +
  ggplot2::scale_y_discrete(expand = expansion(mult = 0.5)) +
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1)) +
  ggplot2::labs(
    x = "Turnout Rate",
    y = NULL
  ) +
  ggplot2::theme_minimal()

turnout_graph <- plotly::ggplotly(
  turnout_graph,
  tooltip = c("text")
)

turnout_graph
