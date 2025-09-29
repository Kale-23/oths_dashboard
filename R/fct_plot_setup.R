#' plot_setup
#'
#' @description sets up shiny output for plotly
#'
#' @noRd
plotly_timeseries <- function(df, new_col_name, seasonal = FALSE) {
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
    marker = list(size = 6),
    #fmt: skip
    text = ~ paste(
      "Location: ", series, "<br>",
      "Value: ", value
    ),
    connectgaps = TRUE # mainly helps with seasonal data
  ) |>
    plotly::layout(
      yaxis = list(title = new_col_name), #TODO col_names_conversions()[[new_col_name]])
      xaxis = list(
        title = "Date",
        type = 'date',
        tickformat = "%d %B<br>%Y"
      )
    )
}

prepare_plot_data <- function(data, col) {
  data |>
    dplyr::select(DateTime, site, value = all_of(col)) |>
    tidyr::pivot_wider(
      names_from = "site",
      values_from = "value",
      values_fn = mean
    ) |>
    dplyr::arrange(DateTime)
}
