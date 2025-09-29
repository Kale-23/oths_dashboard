#'import_individual
#'
#' @description imports individual csv file in the standard format
#'
#' @return csv file
#'
#' @noRd
import_individual <- function(path, site, column_names) {
  readr::read_csv(
    path,
    col_names = column_names,
    skip = 4,
    na = c("NaN", "NAN")
  ) |>
    dplyr::mutate(Site = site)
}

import_names <- function() {
  c(
    "Site",
    "DateTime",
    "date",
    "Year",
    "Month",
    "Day",
    "Hour",
    "AirT_corr",
    "RH_corr",
    "TSoil_5_corr",
    "TSoil_25_corr",
    "VWC_5_corr",
    "VWC_25_corr",
    "ARD_AirT_corr",
    "ARD_RH_corr",
    "ARD_SoilT_corr",
    "ARD_VWC_corr",
    "SnowDepth",
    "SD",
    "ARD_SnowDepth",
    "ARD_SD",
    "SpeCon_5",
    "SpeCon_25"
  )
}

import_renames <- function() {
  c(
    "Site",
    "DateTime",
    "date",
    "Year",
    "Month",
    "Day",
    "Hour",
    "AirT",
    "RH",
    "TSoil_5",
    "TSoil_25",
    "VWC_5",
    "VWC_25",
    "ARD_AirT",
    "ARD_RH",
    "ARD_TSoil",
    "ARD_VWC",
    "SnowDepth",
    "SD_0",
    "ARD_SnowDepth",
    "ARD_SD_0",
    "SpeCon_5",
    "SpeCon_25"
  )
}

dygraph_col_name_conversions <- function() {
  list(
    "AirT" = "Air Temperature (°C)",
    "RH" = "Relative Humidity (%)",
    "SnowDepth" = "Snow Depth (cm)",
    "sdep" = "Soil Temperature (°C)",
    "swe" = "Snow Water Equivalent (mm)",
    "fdep" = "Frost Depth (cm)",
    "TSoil" = "Soil Temperature (°C)",
    "VWC" = "Soil Moisture (%)",
    "SpeCon" = "Soil Specific Conductance (µS/cm)",
    "gcc" = "Daily gcc (90th Percentile)"
  )
}

sensor_renames <- function() {
  c(
    "os" = "Open Source (Arduino)",
    "prop" = "Proprietary",
    "google" = "Google Sheets"
  )
}

site_renames <- function() {
  c(
    "HW" = "Beech",
    "SW" = "Hemlock"
  )
}

single_col_plot <- function(data, column) {
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

  # return dygraph output
  #dygraph_setup(plot_data, column)
  plot_data <- prepare_plot_data(plot_data, column)
  plotly_timeseries(plot_data, column)
}

##numeric_hist <- function(df) {
##  # takes all numeric columns of df and creates a histogram for each column
##  df |>
##    dplyr::select(where(is.numeric)) |>
##    tidyr::pivot_longer(everything()) |>
##    ggplot(aes(x = value)) +
##    geom_histogram(na.rm = TRUE) +
##    facet_wrap(~name, scales = "free")
##}
##
##factor_bar <- function(df) {
##  # takes all factor columns of df and creates a bar plot for each column
##  df |>
##    select(where(is.factor)) |>
##    pivot_longer(everything()) |>
##    count(name, value) |>
##    ggplot(aes(x = value, y = n)) +
##    geom_bar(stat = "identity", na.rm = TRUE) +
##    facet_wrap(~name, scales = "free") +
##    theme(axis.text.x = element_text(size = 5, angle = 45))
##}
##
##missing_plot <- function(df) {
##  df |>
##    mutate(across(everything(), as.character)) |>
##    pivot_longer(everything()) |>
##    mutate(
##      is_missing = is.na(value),
##      name = as.factor(name)
##    ) |>
##    group_by(name, is_missing) |>
##    summarise(missing = n(), .groups = "drop") |>
##    group_by(name) |>
##    mutate(total_missing = sum(missing[is_missing])) |> # extract count of missing values
##    ungroup() |>
##    mutate(
##      name = forcats::fct_reorder(name, total_missing),
##      is_missing = factor(is_missing, levels = c(TRUE, FALSE))
##    ) |>
##    ggplot(aes(x = name, y = missing, fill = is_missing)) +
##    geom_col() +
##    labs(x = "Column", y = "Proportion of Total Columns", fill = "Is Missing") +
##    scale_fill_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
##    theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1))
##}
##
##full_explore_output <- function(df, output_file) {
##  # nicely formats exploratory plots
##  df_hist <- numeric_hist(df)
##  df_bar <- factor_bar(df)
##  df_missing <- missing_plot(df)
##  output_plot <- cowplot::plot_grid(df_missing, df_hist, df_bar, ncol = 1)
##  ggsave(
##    filename = output_file,
##    plot = output_plot,
##    width = 20,
##    height = 30
##  )
##}
##
