#' plot_setup
#'
#' @description sets up shiny output for plotly
#'
#' @noRd
plotly_timeseries <- function(df, new_col_name) {
  # Extract palette
  colors <- as.character(paletteer::paletteer_d("yarrr::info2"))

  # Convert wide data into long for Plotly
  df_long <- tidyr::pivot_longer(
    df,
    cols = -DateTime,
    names_to = "series",
    values_to = "value"
  )

  # Build the plot
  plotly::plot_ly(
    data = df_long,
    x = ~DateTime,
    y = ~value,
    color = ~series,
    colors = colors,
    type = "scatter",
    mode = "lines+markers",
    opacity = 0.7,
    marker = list(size = 1),
    #fmt: skip
    text = ~ paste(
      "Location: ", series, "<br>",
      "Value: ", value
    )
    #connectgaps = TRUE
  ) |>
    plotly::layout(
      yaxis = list(title = plot_name_conversions()[[new_col_name]]),
      xaxis = list(
        title = "Date",
        type = 'date',
        tickformat = "%d %B<br>%Y"
      )
    )
}

prep_plot <- function(data, column) {
  #TODO is this how it should be handled
  if ("sensor" %in% colnames(data)) {
    plot_data <- data |>
      dplyr::summarise(n = mean(.data[[column]]), .by = c(DateTime, sensor, Site)) |>
      tidyr::pivot_wider(
        id_cols = DateTime,
        names_from = c(sensor, Site),
        values_from = n
      ) |>
      dplyr::select(DateTime, dplyr::matches("os|prop|google")) |>
      dplyr::rename_with(~ gsub("os_HW", "Open Source Beech", .x)) |>
      dplyr::rename_with(~ gsub("os_SW", "Open Source Hemlock", .x)) |>
      dplyr::rename_with(~ gsub("prop_HW", "Proprietary Beech", .x)) |>
      dplyr::rename_with(~ gsub("prop_SW", "Proprietary Hemlock", .x)) |>
      dplyr::rename_with(~ gsub("google_HW", "Google Sheets Beech", .x)) |>
      dplyr::rename_with(~ gsub("google_SW", "Google Sheets Hemlock", .x))
  } else {
    plot_data <- data |>
      dplyr::summarise(n = mean(.data[[column]]), .by = c(DateTime, site)) |>
      tidyr::pivot_wider(
        id_cols = DateTime,
        names_from = site,
        values_from = n
      ) |>
      dplyr::rename_with(~ gsub("HW", "Beech", .x)) |>
      dplyr::rename_with(~ gsub("SW", "Hemlock", .x))
  }

  plot_data
}
