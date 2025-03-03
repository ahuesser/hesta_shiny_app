# 00 RAW DATA ----

# 1.0 LOAD LIBRARIES ----

library(BFS)

# 2.0 GET BFS CATALOG DATA ----

catalog <- bfs_get_catalog_data(
  language = "de",
  extended_search = "Hotellerie"
)

# 3.0 SUPPLY AND DEMAND ----

# 3.1 HESTA TOURIST REGION TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_203"
) |>
  saveRDS("01_raw_data/hesta_tourist_region_tbl.rds")

# 3.2 HESTE CANTON TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_202"
) |>
  saveRDS("01_raw_data/hesta_canton_tbl.rds")

# 3.3 HESTA COMMUNE TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_201"
) |>
  saveRDS("01_raw_data/hesta_commune_tbl.rds")

# 4.0 HESTA ORIGIN ----

# 4.1 HESTA TOURIST REGION ORIGIN TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_103"
) |>
  saveRDS("01_raw_data/hesta_tourist_region_origin_tbl.rds")

# 4.2 HESTA CANTON ORIGIN TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_102"
) |>
  saveRDS("01_raw_data/hesta_canton_origin_tbl.rds")

# 4.3 HESTA COMMUNE ORIGIN TBL ----

bfs_get_data(
  language = "de",
  number_bfs = "px-x-1003020000_101"
) |>
  saveRDS("01_raw_data/hesta_commune_origin_tbl.rds")

# 5.0 BFS BASE MAPS ----

# 5.1 SWITZERLAND SF ----

bfs_get_base_maps(geom = "suis") |>
  saveRDS("01_raw_data/switzerland_sf.rds")

# 5.2 TOURIST REGION SF ----

bfs_get_base_maps(geom = "tour") |>
  saveRDS("01_raw_data/tourist_region_sf.rds")

# 5.3 CANTON SF ----

bfs_get_base_maps(geom = "kant") |>
  saveRDS("01_raw_data/canton_sf.rds")

# 5.4 COMMUNE SF ----

bfs_get_base_maps(geom = "polg") |>
  saveRDS("01_raw_data/commune_sf.rds")

# 5.5 LAKE SF ----

bfs_get_base_maps(geom = "seen", category = 11) |>
  saveRDS("01_raw_data/lake_sf.rds")
