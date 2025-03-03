# 02 ECHARTS4R ----

# 1.0 LIBRARIES ----

library(tidyverse)
library(echarts4r)

# 2.0 LOAD DATA ----

hesta_region_tbl <- readRDS("02_processed_data/hesta_region_tbl.rds")

# 3.0 PLOT LINE CHART ----

input_region <- "Kanton St. Gallen"
input_year <- 2024
input_comparison_year <- 2023
input_month <- c("Januar", "Februar", "März", "April")
input_indicator <- "Logiernaechte"

line_chart <- hesta_region_tbl |>
  filter(Region == input_region) |>
  filter(Jahr %in% c(input_year, input_comparison_year)) |>
  filter(Monat %in% input_month) |>
  select(Jahr, Monat, all_of(input_indicator)) |>
  pivot_wider(
    names_from = Jahr,
    values_from = all_of(input_indicator)
  ) |>
  mutate(Monat = substr(Monat, 1, 3)) |>
  drop_na()

line_chart

plot_line_chart <- line_chart |>
  e_charts_(names(line_chart[1])) |>
  e_line_(names(line_chart[2])) |>
  e_line_(names(line_chart[3])) |>
  e_tooltip(trigger = "axis") |>
  e_datazoom(x_index = c(0, 1), toolbox = FALSE) |>
  e_toolbox_feature(feature = "saveAsImage") |>
  e_toolbox_feature(feature = "dataView") |>
  e_locale("CH")

if (input_indicator %in% c("Bettenauslastung", "Zimmerauslastung")) {
  plot_line_chart <- plot_line_chart |>
    e_y_axis(formatter = e_axis_formatter("percent")) |>
    e_tooltip(formatter = e_tooltip_item_formatter("percent"))
}

plot_line_chart

# 4.0 PLOT LINE CHART FUNCTION ----

plot_line_chart <- function(
  data,
  input_region,
  input_year,
  input_comparison_year,
  input_month,
  input_indicator
) {
  line_chart <- data |>
    filter(Region == input_region) |>
    filter(Jahr %in% c(input_year, input_comparison_year)) |>
    filter(Monat %in% input_month) |>
    select(Jahr, Monat, all_of(input_indicator)) |>
    pivot_wider(
      names_from = Jahr,
      values_from = all_of(input_indicator)
    ) |>
    mutate(Monat = substr(Monat, 1, 3)) |>
    drop_na()

  line_chart <- line_chart |>
    e_charts_(names(line_chart[1])) |>
    e_line_(names(line_chart[2])) |>
    e_line_(names(line_chart[3])) |>
    e_tooltip(trigger = "axis") |>
    e_datazoom(x_index = c(0, 1), toolbox = FALSE) |>
    e_toolbox_feature(feature = "saveAsImage") |>
    e_toolbox_feature(feature = "dataView") |>
    e_locale("CH")

  if (input_indicator %in% c("Bettenauslastung", "Zimmerauslastung")) {
    line_chart <- line_chart |>
      e_y_axis(formatter = e_axis_formatter("percent")) |>
      e_tooltip(formatter = e_tooltip_item_formatter("percent"))
  }

  return(line_chart)
}

# 5.0 TEST LINE CHART FUNCTION ----

plot_line_chart(
  hesta_region_tbl,
  "Schweiz",
  2024,
  2023,
  c("Oktober", "November", "Dezember"),
  "Logiernaechte"
)

# 6.0 PLOT BAR CHART ----

input_region <- "Kanton St. Gallen"
input_comparison_region <- "Region Graubünden"
input_year <- 2024
input_comparison_year <- 2023
input_month <- c("Januar", "Februar")
input_indicator <- "Logiernaechte"

bar_chart <- hesta_region_tbl |>
  filter(Region %in% c(input_region, input_comparison_region)) |>
  filter(Jahr %in% c(input_year, input_comparison_year)) |>
  filter(Monat %in% input_month) |>
  select(Jahr, Monat, Region, all_of(input_indicator)) |>
  group_by(Region, Monat) |>
  mutate(
    lag_indicator = lag(!!sym(input_indicator), n = 1, order_by = Jahr),
    Veränderung = (!!sym(input_indicator) / lag_indicator) - 1,
  ) |>
  filter(Jahr == input_year) |>
  ungroup() |>
  select(Monat, Region, Veränderung) |>
  pivot_wider(
    names_from = Region,
    values_from = Veränderung
  ) |>
  mutate(Monat = substr(Monat, 1, 3)) |>
  select(Monat, all_of(input_region), all_of(input_comparison_region))

bar_chart

plot_bar_chart <- bar_chart |>
  e_charts_(names(bar_chart[1])) |>
  e_bar_(names(bar_chart[2])) |>
  e_bar_(names(bar_chart[3])) |>
  e_y_axis(formatter = e_axis_formatter("percent")) |>
  e_tooltip(formatter = e_tooltip_item_formatter("percent", digits = 1)) |>
  e_datazoom(x_index = c(0, 1), toolbox = FALSE) |>
  e_toolbox_feature(feature = "saveAsImage") |>
  e_toolbox_feature(feature = "dataView") |>
  e_locale("CH")

plot_bar_chart

# 7.0 PLOT BAR CHART FUNCTION ----

plot_bar_chart <- function(
  data,
  input_region,
  input_comparison_region,
  input_year,
  input_comparison_year,
  input_month,
  input_indicator
) {
  bar_chart <- data |>
    filter(Region %in% c(input_region, input_comparison_region)) |>
    filter(Jahr %in% c(input_year, input_comparison_year)) |>
    filter(Monat %in% input_month) |>
    select(Jahr, Monat, Region, all_of(input_indicator)) |>
    group_by(Region, Monat) |>
    mutate(
      lag_indicator = lag(!!sym(input_indicator), n = 1, order_by = Jahr),
      Veränderung = (!!sym(input_indicator) / lag_indicator) - 1,
    ) |>
    filter(Jahr == input_year) |>
    ungroup() |>
    select(Monat, Region, Veränderung) |>
    pivot_wider(
      names_from = Region,
      values_from = Veränderung
    ) |>
    mutate(Monat = substr(Monat, 1, 3)) |>
    select(Monat, all_of(input_region), all_of(input_comparison_region))

  bar_chart |>
    e_charts_(names(bar_chart[1])) |>
    e_bar_(names(bar_chart[2])) |>
    e_bar_(names(bar_chart[3])) |>
    e_y_axis(formatter = e_axis_formatter("percent")) |>
    e_tooltip(formatter = e_tooltip_item_formatter("percent", digits = 1)) |>
    e_datazoom(x_index = c(0, 1), toolbox = FALSE) |>
    e_toolbox_feature(feature = "saveAsImage") |>
    e_toolbox_feature(feature = "dataView") |>
    e_locale("CH")
}

# 8.0 TEST BAR CHART FUNCTION ----

plot_bar_chart(
  hesta_region_tbl,
  "Gemeinde Zermatt",
  "Region Wallis",
  2024,
  2023,
  c("Januar", "Februar"),
  "Logiernaechte"
)

# 7.0 DUMP FUNCTIONS ----

dump(
  list = c(
    "plot_line_chart",
    "plot_bar_chart"
  ),
  file = "03_helper_functions/helper_functions.R",
  append = TRUE
)
