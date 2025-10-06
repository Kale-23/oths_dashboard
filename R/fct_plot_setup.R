#' plot_setup
#'
#' @description sets up shiny output for plotly
#'
#' @noRd
plotly_timeseries <- function(df, new_col_name) {
  # Extract palette
  #colors <- as.character(paletteer::paletteer_d("yarrr::info2"))
  colors <- as.character(paletteer::paletteer_d("ggthemes::Classic_10"))

  # Convert wide data into long for Plotly
  df_long <- tidyr::pivot_longer(
    df,
    cols = -DateTime,
    names_to = "series",
    values_to = "value"
  )

  df_long <- dplyr::arrange(df_long, DateTime)

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
    marker = list(size = 2),
    #fmt: skip
    text = ~ paste(
      "Location: ", series, "<br>",
      "Value: ", value
    ),
    connectgaps = TRUE
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
  # handle multiple columns that start with the same string
  matched_cols <- grep(paste0("^", column, "_[0-9]*"), names(data), value = TRUE)
  if (length(matched_cols) > 1) {
    data <- data |>
      dplyr::select(!dplyr::any_of(column)) |>
      tidyr::pivot_longer(
        cols = all_of(matched_cols),
        names_to = "depth", # column name will become "Tsoil"
        values_to = column,
        names_pattern = paste0(column, "_?(.*)"),
        names_repair = "minimal"
      ) |>
      dplyr::mutate(
        !!column := ifelse(.data[[column]] == "", column, .data[[column]])
      )
    col_to_use <- column
  } else {
    # single column case
    col_to_use <- column
  }

  # summarise and reshape depending on whether "sensor" exists
  if ("sensor" %in% colnames(data)) {
    plot_data <- data |>
      dplyr::summarise(
        n = mean(.data[[col_to_use]], na.rm = TRUE),
        .by = c(DateTime, sensor, Site, dplyr::any_of("depth"))
      ) |>
      tidyr::pivot_wider(
        id_cols = c(DateTime, dplyr::matches("depth")),
        names_from = c(sensor, Site),
        values_from = n
      ) |>
      dplyr::select(DateTime, dplyr::matches("os|prop|google"), dplyr::everything()) |>
      dplyr::rename_with(~ gsub("os_HW", "Open Source Beech", .x)) |>
      dplyr::rename_with(~ gsub("os_SW", "Open Source Hemlock", .x)) |>
      dplyr::rename_with(~ gsub("prop_HW", "Proprietary Beech", .x)) |>
      dplyr::rename_with(~ gsub("prop_SW", "Proprietary Hemlock", .x)) |>
      dplyr::rename_with(~ gsub("google_HW", "Google Sheets Beech", .x)) |>
      dplyr::rename_with(~ gsub("google_SW", "Google Sheets Hemlock", .x))
  } else {
    plot_data <- data |>
      dplyr::summarise(
        n = mean(.data[[col_to_use]], na.rm = TRUE),
        .by = c(DateTime, Site, dplyr::any_of("depth"))
      ) |>
      tidyr::pivot_wider(
        id_cols = c(DateTime, dplyr::matches("depth")),
        names_from = Site,
        values_from = n
      ) |>
      dplyr::rename_with(~ gsub("HW", "Beech", .x)) |>
      dplyr::rename_with(~ gsub("SW", "Hemlock", .x))
  }

  if ("depth" %in% colnames(plot_data)) {
    plot_data <- plot_data |>
      tidyr::pivot_wider(
        id_cols = c(DateTime),
        names_from = depth, # factor column
        names_glue = "{.value} {depth}", # add suffix to all value cols
        values_from = !c(DateTime, depth), # all except DateTime
      )
  }

  plot_data
}
