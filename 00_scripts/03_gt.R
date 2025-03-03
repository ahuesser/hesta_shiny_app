# 03 GT ----

# 1.0 LIBRARIES ----

library(tidyverse)
library(gt)

# 2.0 LOAD DATA ----

hesta_region_origin_tbl <- readRDS(
  "02_processed_data/hesta_region_origin_tbl.rds"
)

# 03. USER INPUTS ----

input_region = "Kanton St. Gallen"
input_year = 2024
input_comparison_year = 2023
input_month = c("Januar", "Februar")

# 4.0 DATA PREPROCESSING ----

hesta_region_origin_tbl <- hesta_region_origin_tbl |>
  filter(Region == input_region) |>
  filter(Jahr %in% c(input_year, input_comparison_year)) |>
  filter(Monat %in% input_month) |>
  group_by(Jahr, Herkunftsland) |>

  summarise(
    Ankuenfte = sum(Ankuenfte),
    Logiernaechte = sum(Logiernaechte),
    Aufenthaltsdauer = Logiernaechte / Ankuenfte,
    .groups = "drop"
  ) |>

  group_by(Herkunftsland) |>

  mutate(
    Lag_Ankuenfte = lag(Ankuenfte, n = 1, order_by = Jahr),
    Diff_Ankuenfte = Ankuenfte - Lag_Ankuenfte,
    Veraenderung_Ankuenfte = (Ankuenfte / Lag_Ankuenfte) - 1
  ) |>

  mutate(
    Lag_Logiernaechte = lag(Logiernaechte, n = 1, order_by = Jahr),
    Diff_Logiernaechte = Logiernaechte - Lag_Logiernaechte,
    Veraenderung_Logiernaechte = (Logiernaechte / Lag_Logiernaechte) - 1
  ) |>

  mutate(
    Lag_Aufenthaltsdauer = lag(Aufenthaltsdauer, n = 1, order_by = Jahr),
    Diff_Aufenthaltsdauer = Aufenthaltsdauer - Lag_Aufenthaltsdauer,
  ) |>
  ungroup() |>

  group_by(Jahr) |>

  mutate(
    Total_Logiernaechte = sum(Logiernaechte),
    Anteil_Logiernaechte = Logiernaechte / Total_Logiernaechte,
  ) |>
  ungroup() |>

  group_by(Herkunftsland) |>

  mutate(
    Lag_Anteil_Logiernaechte = lag(
      Anteil_Logiernaechte,
      n = 1,
      order_by = Jahr
    ),
    Diff_Anteil_Logiernaechte = Anteil_Logiernaechte - Lag_Anteil_Logiernaechte,
  ) |>
  ungroup() |>

  mutate(
    across(everything(), ~ replace(., is.infinite(.) | is.nan(.), NA))
  ) |>

  filter(Jahr == input_year) |>
  filter(Logiernaechte != 0) |>
  arrange(desc(Logiernaechte))

# 5.0 Table ----

hesta_region_origin_tbl |>
  gt(rowname_col = "Herkunftsland") |>
  cols_hide(
    columns = c(
      "Jahr",
      "Lag_Ankuenfte",
      "Diff_Ankuenfte",
      "Lag_Aufenthaltsdauer",
      "Lag_Logiernaechte",
      "Diff_Logiernaechte",
      "Total_Logiernaechte",
      "Lag_Anteil_Logiernaechte"
    )
  ) |>

  fmt_number(
    columns = c(
      "Ankuenfte",
      "Logiernaechte"
    ),
    sep_mark = " ",
    decimals = 0
  ) |>

  fmt_number(
    columns = "Aufenthaltsdauer",
    decimals = 2
  ) |>

  fmt_number(
    columns = "Diff_Aufenthaltsdauer",
    decimals = 2,
    force_sign = TRUE
  ) |>

  fmt_percent(
    columns = c(
      "Veraenderung_Ankuenfte",
      "Veraenderung_Logiernaechte",
      "Diff_Anteil_Logiernaechte"
    ),
    decimals = 1,
    force_sign = TRUE,
    sep_mark = " "
  ) |>

  fmt_percent(
    columns = "Anteil_Logiernaechte",
    decimals = 1
  ) |>

  cols_add(
    Trend_Ankuenfte = ifelse(
      Veraenderung_Ankuenfte > 0,
      "caret-up",
      "caret-down"
    ),
    Trend_Logiernaechte = ifelse(
      Veraenderung_Logiernaechte > 0,
      "caret-up",
      "caret-down"
    ),
    Trend_Aufenthaltsdauer = ifelse(
      Diff_Aufenthaltsdauer > 0,
      "caret-up",
      "caret-down"
    ),
    Trend_Anteil_Logiernaechte = ifelse(
      Diff_Anteil_Logiernaechte > 0,
      "caret-up",
      "caret-down"
    )
  ) |>

  fmt_icon(
    columns = starts_with("Trend_"),
    fill_color = c(
      "caret-up" = "forestgreen",
      "caret-down" = "red"
    )
  ) |>

  sub_missing(
    columns = c(
      "Aufenthaltsdauer",
      "Diff_Aufenthaltsdauer",
      "Veraenderung_Ankuenfte",
      "Veraenderung_Logiernaechte",
      "Anteil_Logiernaechte",
      "Diff_Anteil_Logiernaechte"
    ),
    missing_text = "nicht anwendbar"
  ) |>

  sub_missing(
    columns = starts_with("Trend_"),
    missing_text = ""
  ) |>

  cols_merge(
    columns = c(
      "Ankuenfte",
      "Veraenderung_Ankuenfte",
      "Trend_Ankuenfte"
    ),
    pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2} {3}</span>"
  ) |>

  cols_merge(
    columns = c(
      "Logiernaechte",
      "Veraenderung_Logiernaechte",
      "Trend_Logiernaechte"
    ),
    pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2} {3}</span>"
  ) |>

  cols_merge(
    columns = c(
      "Aufenthaltsdauer",
      "Diff_Aufenthaltsdauer",
      "Trend_Aufenthaltsdauer"
    ),
    pattern = "{1} Tage<br><span style='font-size: smaller; color: gray;'>{2} Tage {3}</span>"
  ) |>

  cols_merge(
    columns = c(
      "Anteil_Logiernaechte",
      "Diff_Anteil_Logiernaechte",
      "Trend_Anteil_Logiernaechte"
    ),
    pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2}-Punkte {3}</span>"
  ) |>

  cols_label(
    Ankuenfte = "Ankünfte",
    Logiernaechte = "Logiernächte",
    Anteil_Logiernaechte = "Anteil Logiernächte"
  ) |>

  opt_interactive(
    use_search = TRUE,
    pagination_type = "simple",
    use_pagination_info = FALSE,
    page_size_default = 5,
    use_page_size_select = TRUE,
    page_size_values = c(5, 10, 20, 50),
    use_resizers = TRUE,
    use_sorting = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE
  )

# 6.0 TABLE FUNCTION ----

create_table <- function(
  data,
  input_region,
  input_year,
  input_comparison_year,
  input_month
) {
  gt_table <- data |>
    filter(Region == input_region) |>
    filter(Jahr %in% c(input_year, input_comparison_year)) |>
    filter(Monat %in% input_month) |>
    group_by(Jahr, Herkunftsland) |>

    summarise(
      Ankuenfte = sum(Ankuenfte),
      Logiernaechte = sum(Logiernaechte),
      Aufenthaltsdauer = Logiernaechte / Ankuenfte,
      .groups = "drop"
    ) |>

    group_by(Herkunftsland) |>

    mutate(
      Lag_Ankuenfte = lag(Ankuenfte, n = 1, order_by = Jahr),
      Diff_Ankuenfte = Ankuenfte - Lag_Ankuenfte,
      Veraenderung_Ankuenfte = (Ankuenfte / Lag_Ankuenfte) - 1
    ) |>

    mutate(
      Lag_Logiernaechte = lag(Logiernaechte, n = 1, order_by = Jahr),
      Diff_Logiernaechte = Logiernaechte - Lag_Logiernaechte,
      Veraenderung_Logiernaechte = (Logiernaechte / Lag_Logiernaechte) - 1
    ) |>

    mutate(
      Lag_Aufenthaltsdauer = lag(Aufenthaltsdauer, n = 1, order_by = Jahr),
      Diff_Aufenthaltsdauer = Aufenthaltsdauer - Lag_Aufenthaltsdauer,
    ) |>
    ungroup() |>

    group_by(Jahr) |>

    mutate(
      Total_Logiernaechte = sum(Logiernaechte),
      Anteil_Logiernaechte = Logiernaechte / Total_Logiernaechte,
    ) |>
    ungroup() |>

    group_by(Herkunftsland) |>

    mutate(
      Lag_Anteil_Logiernaechte = lag(
        Anteil_Logiernaechte,
        n = 1,
        order_by = Jahr
      ),
      Diff_Anteil_Logiernaechte = Anteil_Logiernaechte -
        Lag_Anteil_Logiernaechte,
    ) |>
    ungroup() |>

    mutate(
      across(everything(), ~ replace(., is.infinite(.) | is.nan(.), NA))
    ) |>

    filter(Jahr == input_year) |>
    filter(Logiernaechte != 0) |>
    arrange(desc(Logiernaechte))

  gt_table |>
    gt(rowname_col = "Herkunftsland") |>
    cols_hide(
      columns = c(
        "Jahr",
        "Lag_Ankuenfte",
        "Diff_Ankuenfte",
        "Lag_Aufenthaltsdauer",
        "Lag_Logiernaechte",
        "Diff_Logiernaechte",
        "Total_Logiernaechte",
        "Lag_Anteil_Logiernaechte"
      )
    ) |>

    fmt_number(
      columns = c(
        "Ankuenfte",
        "Logiernaechte"
      ),
      sep_mark = " ",
      decimals = 0
    ) |>

    fmt_number(
      columns = "Aufenthaltsdauer",
      decimals = 2
    ) |>

    fmt_number(
      columns = "Diff_Aufenthaltsdauer",
      decimals = 2,
      force_sign = TRUE
    ) |>

    fmt_percent(
      columns = c(
        "Veraenderung_Ankuenfte",
        "Veraenderung_Logiernaechte",
        "Diff_Anteil_Logiernaechte"
      ),
      decimals = 1,
      force_sign = TRUE,
      sep_mark = " "
    ) |>

    fmt_percent(
      columns = "Anteil_Logiernaechte",
      decimals = 1
    ) |>

    cols_add(
      Trend_Ankuenfte = ifelse(
        Veraenderung_Ankuenfte > 0,
        "caret-up",
        "caret-down"
      ),
      Trend_Logiernaechte = ifelse(
        Veraenderung_Logiernaechte > 0,
        "caret-up",
        "caret-down"
      ),
      Trend_Aufenthaltsdauer = ifelse(
        Diff_Aufenthaltsdauer > 0,
        "caret-up",
        "caret-down"
      ),
      Trend_Anteil_Logiernaechte = ifelse(
        Diff_Anteil_Logiernaechte > 0,
        "caret-up",
        "caret-down"
      )
    ) |>

    fmt_icon(
      columns = starts_with("Trend_"),
      fill_color = c(
        "caret-up" = "forestgreen",
        "caret-down" = "red"
      )
    ) |>

    sub_missing(
      columns = c(
        "Aufenthaltsdauer",
        "Diff_Aufenthaltsdauer",
        "Veraenderung_Ankuenfte",
        "Veraenderung_Logiernaechte",
        "Anteil_Logiernaechte",
        "Diff_Anteil_Logiernaechte"
      ),
      missing_text = "nicht anwendbar"
    ) |>

    sub_missing(
      columns = starts_with("Trend_"),
      missing_text = ""
    ) |>

    cols_merge(
      columns = c(
        "Ankuenfte",
        "Veraenderung_Ankuenfte",
        "Trend_Ankuenfte"
      ),
      pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2} {3}</span>"
    ) |>

    cols_merge(
      columns = c(
        "Logiernaechte",
        "Veraenderung_Logiernaechte",
        "Trend_Logiernaechte"
      ),
      pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2} {3}</span>"
    ) |>

    cols_merge(
      columns = c(
        "Aufenthaltsdauer",
        "Diff_Aufenthaltsdauer",
        "Trend_Aufenthaltsdauer"
      ),
      pattern = "{1} Tage<br><span style='font-size: smaller; color: gray;'>{2} Tage {3}</span>"
    ) |>

    cols_merge(
      columns = c(
        "Anteil_Logiernaechte",
        "Diff_Anteil_Logiernaechte",
        "Trend_Anteil_Logiernaechte"
      ),
      pattern = "{1}<br><span style='font-size: smaller; color: gray;'>{2}-Punkte {3}</span>"
    ) |>

    cols_label(
      Ankuenfte = "Ankünfte",
      Logiernaechte = "Logiernächte",
      Anteil_Logiernaechte = "Anteil Logiernächte"
    ) |>

    opt_interactive(
      use_search = TRUE,
      pagination_type = "simple",
      use_pagination_info = FALSE,
      page_size_default = 5,
      use_page_size_select = TRUE,
      page_size_values = c(5, 10, 20, 50),
      use_resizers = TRUE,
      use_sorting = TRUE,
      use_highlight = TRUE,
      use_compact_mode = TRUE
    )
}

# 7.0 TEST FUNCTION ----

create_table(
  hesta_region_origin_tbl,
  "Kanton St. Gallen",
  2024,
  2023,
  c("Dezember", "Januar", "Februar")
)

# 8.0 DUMP FUNCTION ----

dump(
  "create_table",
  file = "03_helper_functions/helper_functions.R",
  append = TRUE
)
