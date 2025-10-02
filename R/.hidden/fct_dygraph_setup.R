#' dygraph_setup
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
dygraph_setup <- function(data, column) {
  # filter out rows where all columns except DateTime are NA
  data <- data |>
    dplyr::filter(dplyr::if_any(-DateTime, ~ !is.na(.)))
  # convert date to Date class and filter out NAs
  df_index <- as.Date(data$DateTime)
  df_index <- df_index[!is.na(df_index)]
  #TODO fix this
  new_col_name <- dygraph_col_name_conversions()[[column]]

  # select only relevant columns and filter out NAs
  dy_data <- data |>
    dplyr::filter(!is.na(DateTime)) |>
    dplyr::select(-DateTime)

  # convert to xts object and plot
  if (nrow(dy_data) == 0) {
    return(NULL)
  }
  df_xts <- xts::xts(dy_data, order.by = df_index)

  dygraphs::dygraph(df_xts, group = "dygraph") |>
    dygraphs::dyAxis("y", label = new_col_name) |>
    dygraphs::dyRangeSelector(height = 20) |>
    dygraphs::dyLegend(width = 500) |>
    dygraphs::dyOptions(
      connectSeparatedPoints = TRUE,
      drawPoints = TRUE,
      pointSize = 2,
      colors = paletteer::paletteer_d(
        "yarrr::info2",
      )
    ) |>
    dygraphs::dyCallbacks(drawCallback = dyRegister())
}
