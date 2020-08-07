load(file = "./data/global.RData")

library("tidyverse")
library("shinythemes")
library("shinyWidgets")
library("hrbrthemes")
library("showtext")
library("shiny")
library("tidyverse")
library("ggplot2")
library("viridis")
library("leaflet")
library("treemapify")
library("ggrepel")

# hrbrthemes::import_roboto_condensed()

font_add_google(
  name = "Roboto Condensed",
  family = "Roboto Condensed",
  regular.wt = 400,
  bold.wt = 700
)

showtext_auto()

showtext_opts(dpi = 112)

#hrbrthemes.loadfonts = T
