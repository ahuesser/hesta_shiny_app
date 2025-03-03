# 04 LEAFLET ----

# 1.0 LIBRARIES ----

library(kableExtra)
library(BFS)
library(tidyverse)
library(sf)
library(leaflet)

# 2.0 LOAD DATA ----

hesta_region_origin_tbl <- readRDS(
  "02_processed_data/hesta_region_origin_tbl.rds"
)

region_sf <- readRDS(
  "02_processed_data/region_sf.rds"
)

lake_sf <- readRDS(
  "02_processed_data/lake_sf.rds"
)

# 3.0 USER INPUTS ----

analysis_level <- "Tourismusregionen"
input_year <- 2024
input_month <- c("Januar", "Februar")
input_indicator <- "Ankuenfte"

# 4.0 PREPROCESSING ----

prefix <- switch(
  analysis_level,
  "Tourismusregionen" = "Region ",
  "Kantone" = "Kanton ",
  "Gemeinden" = "Gemeinde "
)

region_indicator <- hesta_region_origin_tbl |>
  filter(str_starts(Region, prefix)) |>
  filter(Jahr == input_year) |>
  filter(Monat %in% input_month) |>
  group_by(Jahr, Region) |>
  summarise(
    Wert = sum(.data[[input_indicator]]),
    .groups = "drop"
  ) |>
  group_by(Jahr) |>
  mutate(
    Total_Wert = sum(Wert),
    Anteil_Wert = (Wert / Total_Wert) * 100
  ) |>
  ungroup()

region_indicator

# 5.0 TOP 5 ORIGINS ----

top_5_origins <- hesta_region_origin_tbl |>
  filter(str_starts(Region, prefix)) |>
  filter(Jahr == input_year) |>
  filter(Monat %in% input_month) |>
  group_by(Jahr, Region, Herkunftsland) |>
  summarise(
    Wert = sum(.data[[input_indicator]]),
    .groups = "drop"
  ) |>
  arrange(Region, desc(Wert)) |>
  group_by(Jahr, Region) |>
  slice_head(n = 5) |>
  ungroup()

top_5_origins

# 6.0 CREATE HTML TABLE ----

input_indicator <- switch(
  input_indicator,
  "Logiernaechte" = "Logiernächte",
  "Ankuenfte" = "Ankünfte"
)

region_table <- function(region, indicator = input_indicator) {
  df <- top_5_origins |>
    filter(Region == region)

  df_renamed <- df |>
    rename(!!input_indicator := Wert)

  df_renamed[[input_indicator]] <- format(
    df_renamed[[input_indicator]],
    big.mark = " ",
    scientific = FALSE
  )

  table <- df_renamed |>
    select(Herkunftsland, all_of(input_indicator)) |>
    kableExtra::kable(
      format = "html",
      align = "lr",
      col.names = c("Herkunftsland", input_indicator)
    )

  paste0(
    "<b>",
    unique(df$Region),
    "</b><br>",
    table
  )
}

region_table("Region Zürich")

# 7.0 REGION WITH HTML TABLES ----

region_with_tables <- top_5_origins |>
  mutate(
    Tabelle = map_chr(Region, region_table)
  ) |>
  group_by(Region) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(
    Jahr,
    Region,
    Tabelle
  )

region_with_tables

# 8.0 JOIN INDICATOR WITH HTML TABLE ----

region_indicator <- left_join(
  region_indicator,
  region_with_tables,
  by = c("Jahr", "Region")
)

region_indicator

# 9.0 JOIN SHAPE FILES ----

hesta_region_joined_sf <- left_join(
  region_indicator,
  region_sf,
  by = c("Region")
) |>
  st_as_sf()

hesta_region_joined_sf

# 10.0 MAP ----

bins = c(0, 10, 15, 20, 25, 30, 35)

col_custom <- c(
  "#9ad08f",
  "#81bd7a",
  "#68aa64",
  "#4f964f",
  "#368339",
  "#1d7024"
)

pal <- colorBin(
  col_custom,
  domain = hesta_region_joined_sf$Anteil_Wert,
  bins = bins
)

legend_labels <- c(
  "0 – 10%",
  "10 – 15%",
  "15 – 20%",
  "20 – 25%",
  "25 – 30%",
  "30 – 35%"
)

bbox <- st_bbox(hesta_region_joined_sf) |>
  as.vector()

region_map <- leaflet(hesta_region_joined_sf) |>
  addProviderTiles(
    provider = "OpenStreetMap",
    options = providerTileOptions(
      minZoom = 8,
      maxZoom = 12
    )
  ) |>

  addPolygons(
    data = hesta_region_joined_sf,
    stroke = TRUE,
    color = "black",
    weight = 0.5,
    fillColor = ~ pal(Anteil_Wert),
    fillOpacity = 0.7,
    label = ~ lapply(Tabelle, htmltools::HTML),
    highlightOptions = highlightOptions(
      color = "blue",
      weight = 3,
      bringToFront = TRUE
    )
  ) |>

  addPolygons(
    data = lake_sf,
    stroke = FALSE,
    fillOpacity = 0.1,
    label = ~name
  ) |>

  addLegend(
    title = paste0("Anteil am Total der ", input_indicator, " in %"),
    labFormat = function(type, cuts, p) {
      paste0(legend_labels)
    },
    values = ~Anteil_Wert,
    pal = pal,
    opacity = 1
  ) |>

  setMaxBounds(
    lng1 = bbox[1],
    lat1 = bbox[2],
    lng2 = bbox[3],
    lat2 = bbox[4]
  ) |>

  leaflet.extras::setMapWidgetStyle(
    style = list(background = "transparent")
  )

region_map

# 11.0 MAPPING FUNCTION ----

create_map <- function(
  data,
  analysis_level,
  input_year,
  input_month,
  input_indicator
) {
  prefix <- switch(
    analysis_level,
    "Tourismusregionen" = "Region ",
    "Kantone" = "Kanton ",
    "Gemeinden" = "Gemeinde "
  )

  region_indicator <- data |>
    filter(str_starts(Region, prefix)) |>
    filter(Jahr == input_year) |>
    filter(Monat %in% input_month) |>
    group_by(Jahr, Region) |>
    summarise(
      Wert = sum(.data[[input_indicator]]),
      .groups = "drop"
    ) |>
    group_by(Jahr) |>
    mutate(
      Total_Wert = sum(Wert),
      Anteil_Wert = (Wert / Total_Wert) * 100
    ) |>
    ungroup()

  top_5_origins <- hesta_region_origin_tbl |>
    filter(str_starts(Region, prefix)) |>
    filter(Jahr == input_year) |>
    filter(Monat %in% input_month) |>
    group_by(Jahr, Region, Herkunftsland) |>
    summarise(
      Wert = sum(.data[[input_indicator]]),
      .groups = "drop"
    ) |>
    arrange(Region, desc(Wert)) |>
    group_by(Jahr, Region) |>
    slice_head(n = 5) |>
    ungroup()

  input_indicator <- switch(
    input_indicator,
    "Logiernaechte" = "Logiernächte",
    "Ankuenfte" = "Ankünfte"
  )

  region_table <- function(region, indicator = input_indicator) {
    df <- top_5_origins |>
      filter(Region == region)

    df_renamed <- df |>
      rename(!!input_indicator := Wert)

    df_renamed[[input_indicator]] <- format(
      df_renamed[[input_indicator]],
      big.mark = " ",
      scientific = FALSE
    )

    table <- df_renamed |>
      select(Herkunftsland, all_of(input_indicator)) |>
      kableExtra::kable(
        format = "html",
        align = "lr"
      )

    paste0(
      "<b>",
      unique(df$Region),
      "</b><br>",
      table
    )
  }

  region_with_tables <- top_5_origins |>
    mutate(
      Tabelle = map_chr(Region, region_table)
    ) |>
    group_by(Region) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(
      Jahr,
      Region,
      Tabelle
    )

  region_indicator <- left_join(
    region_indicator,
    region_with_tables,
    by = c("Jahr", "Region")
  )

  hesta_region_joined_sf <- left_join(
    region_indicator,
    region_sf,
    by = c("Region")
  ) |>
    st_as_sf()

  bins = c(0, 10, 15, 20, 25, 30, 35)

  col_custom <- c(
    "#9ad08f",
    "#81bd7a",
    "#68aa64",
    "#4f964f",
    "#368339",
    "#1d7024"
  )

  pal <- colorBin(
    col_custom,
    domain = hesta_region_joined_sf$Anteil_Wert,
    bins = bins
  )

  legend_labels <- c(
    "0 – 10%",
    "10 – 15%",
    "15 – 20%",
    "20 – 25%",
    "25 – 30%",
    "30 – 35%"
  )

  bbox <- st_bbox(hesta_region_joined_sf) |>
    as.vector()

  leaflet(hesta_region_joined_sf) |>
    addProviderTiles(
      provider = "OpenStreetMap",
      options = providerTileOptions(
        minZoom = 8,
        maxZoom = 12
      )
    ) |>

    addPolygons(
      data = hesta_region_joined_sf,
      stroke = TRUE,
      color = "black",
      weight = 0.5,
      fillColor = ~ pal(Anteil_Wert),
      fillOpacity = 0.7,
      label = ~ lapply(Tabelle, htmltools::HTML),
      highlightOptions = highlightOptions(
        color = "blue",
        weight = 3,
        bringToFront = TRUE
      )
    ) |>

    addPolygons(
      data = lake_sf,
      stroke = FALSE,
      fillOpacity = 0.1,
      label = ~name
    ) |>

    addLegend(
      title = paste0("Anteil am Total der ", input_indicator, " in %"),
      labFormat = function(type, cuts, p) {
        paste0(legend_labels)
      },
      values = ~Anteil_Wert,
      pal = pal,
      opacity = 1
    ) |>

    setMaxBounds(
      lng1 = bbox[1],
      lat1 = bbox[2],
      lng2 = bbox[3],
      lat2 = bbox[4]
    ) |>

    leaflet.extras::setMapWidgetStyle(
      style = list(background = "transparent")
    )
}

# 11.0 TEST MAPPING FUNCTION ----

create_map(
  hesta_region_origin_tbl,
  "Gemeinden",
  2024,
  c("Januar", "Februar"),
  "Logiernaechte"
)

# 12.0 DUMP FUNCTION ----

dump(
  "create_map",
  file = "03_helper_functions/helper_functions.R",
  append = TRUE
)
