# 01 PROCESSED DATA ----

# 1.0 LOAD LIBRARIES ----

library(BFS)
library(tidyverse)
library(sf)

# 2.0 LOAD RAW DATA ----

hesta_tourist_region_tbl <- readRDS(
  "01_raw_data/hesta_tourist_region_tbl.rds"
)

hesta_canton_tbl <- readRDS(
  "01_raw_data/hesta_canton_tbl.rds"
)

hesta_commune_tbl <- readRDS(
  "01_raw_data/hesta_commune_tbl.rds"
)

hesta_tourist_region_origin_tbl <- readRDS(
  "01_raw_data/hesta_tourist_region_origin_tbl.rds"
)

hesta_canton_origin_tbl <- readRDS(
  "01_raw_data/hesta_canton_origin_tbl.rds"
)

hesta_commune_origin_tbl <- readRDS(
  "01_raw_data/hesta_commune_origin_tbl.rds"
)

switzerland_sf <- readRDS(
  "01_raw_data/switzerland_sf.rds"
)

tourist_region_sf <- readRDS(
  "01_raw_data/tourist_region_sf.rds"
)

canton_sf <- readRDS(
  "01_raw_data/canton_sf.rds"
)

commune_sf <- readRDS(
  "01_raw_data/commune_sf.rds"
)

lake_sf <- readRDS(
  "01_raw_data/lake_sf.rds"
)

# 3.0 HESTA DATA TBL ----

# 3.1 HESTA TOURIST REGION TBL ----

hesta_tourist_region_tbl <- hesta_tourist_region_tbl |>
  rename(
    Region = Tourismusregion,
    Wert = `Hotellerie: Angebot und Nachfrage der geöffneten Betriebe`
  ) |>
  filter(Monat != "Jahrestotal") |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = str_replace(
      Region,
      "Luzern / Vierwaldstättersee",
      "Luzern/Vierwaldstättersee"
    ),
    Region = str_replace(Region, " Region", ""),
    Region = if_else(
      Region != "Schweiz",
      paste0("Region ", Region),
      Region
    ),
    Indikator = str_replace(Indikator, " in %", "")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte,
    Zimmernaechte = Zimmernächte
  ) |>
  mutate(
    Aufenthaltsdauer = Logiernaechte / Ankuenfte,
    Zimmerauslastung = Zimmerauslastung / 100,
    Bettenauslastung = Bettenauslastung / 100
  )

# 3.2 HESTA CANTON TBL ----

hesta_canton_tbl <- hesta_canton_tbl |>
  rename(
    Region = Kanton,
    Wert = `Hotellerie: Angebot und Nachfrage der geöffneten Betriebe`
  ) |>
  filter(Monat != "Jahrestotal") |>
  filter(Region != "Schweiz") |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = str_replace(Region, "Fribourg", "Freiburg"),
    Region = str_replace(
      Region,
      "Appenzell-Innerrhoden",
      "Appenzell Innerrhoden"
    ),
    Region = str_replace(
      Region,
      "Appenzell-Ausserrhoden",
      "Appenzell Ausserrhoden"
    ),
    Region = str_replace(Region, "Ticino", "Tessin"),
    Region = str_replace(Region, "Vaud", "Waadt"),
    Region = str_replace(Region, "Valais", "Wallis"),
    Region = str_replace(Region, "Neuchâtel", "Neuenburg"),
    Region = str_replace(Region, "Genève", "Genf"),
    Region = paste0("Kanton ", Region),
    Indikator = str_replace(Indikator, " in %", "")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte,
    Zimmernaechte = Zimmernächte
  ) |>
  mutate(
    Aufenthaltsdauer = Logiernaechte / Ankuenfte,
    Zimmerauslastung = Zimmerauslastung / 100,
    Bettenauslastung = Bettenauslastung / 100
  )

# 3.3 HESTA COMMUNE TBL ----

hesta_commune_tbl <- hesta_commune_tbl |>
  rename(
    Region = Gemeinde,
    Wert = `Hotellerie: Angebot und Nachfrage der geöffneten Betriebe`
  ) |>
  filter(Monat != "Jahrestotal") |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = paste0("Gemeinde ", Region),
    Indikator = str_replace(Indikator, " in %", "")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte,
    Zimmernaechte = Zimmernächte
  ) |>
  mutate(
    Aufenthaltsdauer = Logiernaechte / Ankuenfte,
    Zimmerauslastung = Zimmerauslastung / 100,
    Bettenauslastung = Bettenauslastung / 100
  )

# 4.0 ROW BIND DATA AND SAVE RDS ----

rbind(
  hesta_tourist_region_tbl,
  hesta_canton_tbl,
  hesta_commune_tbl
) |>
  saveRDS(file = "02_processed_data/hesta_region_tbl.rds")

# 5.0 HESTA ORIGIN TBL ----

# 5.1 HESTA TOURIST REGION ORIGIN TBL ----

hesta_tourist_region_origin_tbl <- hesta_tourist_region_origin_tbl |>
  rename(
    Region = Tourismusregion,
    Wert = `Hotellerie: Ankünfte und Logiernächte der geöffneten Betriebe`
  ) |>
  filter(Monat != "Jahrestotal") |>
  filter(Herkunftsland != "Herkunftsland - Total") |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = str_replace(
      Region,
      "Luzern / Vierwaldstättersee",
      "Luzern/Vierwaldstättersee"
    ),
    Region = str_replace(Region, " Region", ""),
    Region = if_else(Region != "Schweiz", paste0("Region ", Region), Region),
    Herkunftsland = str_replace(Herkunftsland, "Korea \\(Süd-\\)", "Südkorea")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte
  ) |>
  mutate(Aufenthaltsdauer = Logiernaechte / Ankuenfte)

# 5.2 HESTA CANTON ORIGIN TBL ----

hesta_canton_origin_tbl <- hesta_canton_origin_tbl |>
  rename(
    Region = Kanton,
    Wert = `Hotellerie: Ankünfte und Logiernächte der geöffneten Betriebe`
  ) |>
  filter(
    Monat != "Jahrestotal",
    Herkunftsland != "Herkunftsland - Total",
    Region != "Schweiz"
  ) |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = str_replace(Region, "Bern / Berne", "Bern"),
    Region = str_replace(Region, "Fribourg / Freiburg", "Freiburg"),
    Region = str_replace(
      Region,
      "Graubünden / Grigioni / Grischun",
      "Graubünden"
    ),
    Region = str_replace(Region, "Ticino", "Tessin"),
    Region = str_replace(Region, "Vaud", "Waadt"),
    Region = str_replace(Region, "Valais / Wallis", "Wallis"),
    Region = str_replace(Region, "Neuchâtel", "Neuenburg"),
    Region = str_replace(Region, "Genève", "Genf"),
    Region = paste0("Kanton ", Region),
    Herkunftsland = str_replace(Herkunftsland, "Korea \\(Süd-\\)", "Südkorea")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte
  ) |>
  mutate(Aufenthaltsdauer = Logiernaechte / Ankuenfte)

# 5.3 HESTA COMMUNE ORIGIN TBL ----

hesta_commune_origin_tbl <- hesta_commune_origin_tbl |>
  rename(
    Region = Gemeinde,
    Wert = `Hotellerie: Ankünfte und Logiernächte der geöffneten Betriebe`
  ) |>
  filter(Monat != "Jahrestotal") |>
  filter(Herkunftsland != "Herkunftsland - Total") |>
  mutate(
    Jahr = as.integer(Jahr),
    Region = paste0("Gemeinde ", Region),
    Herkunftsland = str_replace(Herkunftsland, "Korea \\(Süd-\\)", "Südkorea")
  ) |>
  drop_na() |>
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  ) |>
  rename(
    Ankuenfte = Ankünfte,
    Logiernaechte = Logiernächte
  ) |>
  mutate(Aufenthaltsdauer = Logiernaechte / Ankuenfte)

# 6.0 VALIDATION

setequal(
  hesta_tourist_region_tbl$Region,
  hesta_tourist_region_origin_tbl$Region
)

setequal(
  hesta_canton_tbl$Region,
  hesta_canton_origin_tbl$Region
)

setequal(
  hesta_commune_tbl$Region,
  hesta_commune_origin_tbl$Region
)

setequal(
  hesta_tourist_region_origin_tbl$Herkunftsland,
  hesta_canton_origin_tbl$Herkunftsland
)

setequal(
  hesta_canton_origin_tbl$Herkunftsland,
  hesta_commune_origin_tbl$Herkunftsland
)

setdiff(
  hesta_canton_origin_tbl$Herkunftsland,
  hesta_commune_origin_tbl$Herkunftsland
)

# 7.0 RBIND DATA AND SAVE RDS ----

rbind(
  hesta_tourist_region_origin_tbl,
  hesta_canton_origin_tbl,
  hesta_commune_origin_tbl
) |>
  saveRDS("02_processed_data/hesta_region_origin_tbl.rds")

# 8.0 BFS BASE MAPS ----

# 8.1 SWITZERLAND SF ----

switzerland_sf <- switzerland_sf |>
  st_transform(crs = 4326) |>
  rename(Region = name) |>
  mutate(
    Region = str_replace(Region, "Schweiz/Suisse/Svizzera/Svizra", "Schweiz")
  ) |>
  select(-id)

# 8.2 TOURIST REGION SF ----

tourist_region_sf <- tourist_region_sf |>
  st_transform(crs = 4326) |>
  rename(Region = name_tour) |>
  mutate(
    Region = str_replace(Region, "Waadtland", "Waadt"),
    Region = str_replace(
      Region,
      "Luzern / Vierwaldstättersee",
      "Luzern/Vierwaldstättersee"
    ),
    Region = str_replace(
      Region,
      "Aargau Region",
      "Aargau und Solothurn Region"
    ),
    Region = str_replace(Region, " Region", ""),
    Region = paste0("Region ", Region)
  ) |>
  select(-id_tour)

# 8.3 CANTON SF ----

canton_sf <- canton_sf |>
  st_transform(crs = 4326) |>
  rename(Region = name) |>
  mutate(
    Region = str_replace(Region, "Fribourg", "Freiburg"),
    Region = str_replace(Region, "Vaud", "Waadt"),
    Region = str_replace(Region, "Valais", "Wallis"),
    Region = str_replace(Region, "Neuchâtel", "Neuenburg"),
    Region = str_replace(Region, "Genève", "Genf"),
    Region = paste0("Kanton ", Region)
  ) |>
  select(-id)

# 8.4 COMMUNE SF ----

commune_sf <- commune_sf |>
  st_transform(crs = 4326) |>
  rename(Region = name) |>
  mutate(
    Region = paste0("Gemeinde ", Region)
  ) |>
  select(-id)

# 8.5 RBIND SF AND SAVE RDS ----

rbind(
  switzerland_sf,
  tourist_region_sf,
  canton_sf,
  commune_sf
) |>
  saveRDS("02_processed_data/region_sf.rds")

# 8.6 LAKE SF AND SAVE RDS ----

lake_sf |>
  st_transform(crs = 4326) |>
  select(-id) |>
  saveRDS("02_processed_data/lake_sf.rds")

# 9.0 LOAD RDS ----

readRDS("02_processed_data/hesta_region_tbl.rds") |> tail()
readRDS("02_processed_data/hesta_region_origin_tbl.rds") |> tail()
readRDS("02_processed_data/region_sf.rds")
readRDS("02_processed_data/lake_sf.rds")
