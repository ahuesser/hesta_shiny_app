plot_line_chart <-
function(
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
plot_bar_chart <-
function(
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
create_table <-
function(
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
create_map <-
function(
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
