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
    "ARD_SoilT",
    "ARD_VWC",
    "SnowDepth",
    "SD_0",
    "ARD_SnowDepth",
    "ARD_SD_0",
    "SpeCon_5",
    "SpeCon_25"
  )
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
