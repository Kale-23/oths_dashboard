#' import_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
import_data <- function() {
  common = "~/Desktop/"
  #common = "https://hbrsensor.sr.unh.edu/data/snownet/"

  # get column names
  col_names <- colnames(readr::read_csv(
    paste0(common, "OTHSHW_SOIL_MET.dat"),
    n_max = 0,
    skip = 1
  ))

  # current dynamic files
  hw <- import_individual(paste0(common, "OTHSHW_SOIL_MET.dat"), "HW", col_names)
  sw <- import_individual(paste0(common, "OTHSSW_SOIL_MET.dat"), "SW", col_names)

  # old pre ardiuno

  # TODO: move back whenever full data figured out
  common = "https://hbrsensor.sr.unh.edu/data/snownet/"
  hw_old <- import_individual(paste0(common, "OTHSHW_SOIL_MET.dat.backup"), "HW", col_names)
  sw_old <- import_individual(paste0(common, "OTHSSW_SOIL_MET.dat.backup"), "SW", col_names)

  ot <- dplyr::bind_rows(hw, hw_old, sw, sw_old)
  rm(hw, sw, hw_old, sw_old, col_names, common)
  ot <- ot |>
    dplyr::mutate(
      DateTime = TIMESTAMP,
      date = lubridate::as_date(DateTime, tz = "EST"),
      Year = lubridate::year(DateTime),
      Month = lubridate::month(DateTime),
      Day = lubridate::day(DateTime),
      Hour = lubridate::hour(DateTime),
    )

  # quality control (TRUE = good, FALSE = bad)
  ot <- ot |>
    dplyr::mutate(
      batt_flag = dplyr::if_else(batt_volt_Min >= 12, TRUE, FALSE),
      # air temp
      airT_flag = dplyr::if_else(
        dplyr::between(AirT, -40, 60),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_airT_flag = dplyr::if_else(
        dplyr::between(ARD_AirT, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      # relative humidity
      RH_flag = dplyr::if_else(
        dplyr::between(RH, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_RH_flag = dplyr::if_else(
        dplyr::between(ARD_RH, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      # snow depth
      TCDT_flag = dplyr::if_else(
        dplyr::between(TCDT, 0.5, 10),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_DT_flag = dplyr::if_else(
        dplyr::between(ARD_DT, 50, 1940),
        TRUE,
        FALSE,
        missing = NA
      ),
      Q_flag = dplyr::if_else(
        dplyr::between(Q, 152, 210),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil temp
      TSoil_5_flag = dplyr::if_else(
        dplyr::between(TSoil_5, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      TSoil_25_flag = dplyr::if_else(
        dplyr::between(TSoil_25, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_SoilT_flag = dplyr::if_else(
        dplyr::between(ARD_SoilT, -40, 123.8),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil moisture
      VWC_5_flag = dplyr::if_else(
        dplyr::between(VWC_5, 0, 1),
        TRUE,
        FALSE,
        missing = NA
      ),
      VWC_25_flag = dplyr::if_else(
        dplyr::between(VWC_25, 0, 1),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_VWC_flag = dplyr::if_else(
        dplyr::between(ARD_VWC_5, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil electrical conductivity
      EC_5_flag = dplyr::if_else(
        dplyr::between(EC_5, 0, 3),
        TRUE,
        FALSE,
        missing = NA
      ),
      EC_25_flag = dplyr::if_else(
        dplyr::between(EC_25, 0, 3),
        TRUE,
        FALSE,
        missing = NA
      )
    )

  # create corr columns
  ot <- ot |>
    dplyr::mutate(
      AirT_corr = dplyr::if_else(airT_flag, AirT, NA_real_),
      RH_corr = dplyr::if_else(RH_flag, RH, NA_real_),
      TCDT_corr = dplyr::if_else(TCDT_flag & Q_flag, TCDT, NA_real_),
      TSoil_5_corr = dplyr::if_else(TSoil_5_flag, TSoil_5, NA_real_),
      TSoil_25_corr = dplyr::if_else(TSoil_25_flag, TSoil_25, NA_real_),
      VWC_5_corr = dplyr::if_else(VWC_5_flag, VWC_5, NA_real_),
      VWC_25_corr = dplyr::if_else(VWC_25_flag, VWC_25, NA_real_),
      EC_5_corr = dplyr::if_else(EC_5_flag, EC_5, NA_real_),
      EC_25_corr = dplyr::if_else(EC_25_flag, EC_25, NA_real_),

      ARD_AirT_corr = dplyr::if_else(ARD_airT_flag, ARD_AirT, NA_real_),
      ARD_RH_corr = dplyr::if_else(ARD_RH_flag, ARD_RH, NA_real_),
      ARD_DT_corr = dplyr::if_else(ARD_DT_flag, ARD_DT, NA_real_),
      ARD_SoilT_corr = dplyr::if_else(ARD_SoilT_flag, ARD_SoilT, NA_real_),
      ARD_VWC_corr = dplyr::if_else(ARD_VWC_flag, ARD_VWC_5, NA_real_)
    )

  # subtract snow sensor height from TCDT_corr to get snow depth
  # height varies over time as boom was adjusted
  # multiply by 100 to get units in cm
  ot <- ot |>
    dplyr::mutate(
      SD = dplyr::case_when(
        Site == "HW" & DateTime < as.POSIXct("2019-11-15") ~ (1.81 - TCDT_corr) * 100, # min
        Site == "HW" & DateTime >= as.POSIXct("2019-11-15") & DateTime < as.POSIXct("2020-04-01") ~
          (1.80 - TCDT_corr) * 100,
        Site == "HW" & DateTime >= as.POSIXct("2020-04-01") ~ (1.84 - TCDT_corr) * 100, # max
        Site == "SW" & DateTime < as.POSIXct("2020-04-01") ~ (1.84 - TCDT_corr) * 100, # min
        Site == "SW" & DateTime >= as.POSIXct("2020-04-01") ~ (1.88 - TCDT_corr) * 100 # max
      )
    )
  # fmt: skip
  #calibration <- tribble(
  #  ~Site, ~start,                 ~end,                   ~coef,
  #  "HW",  as.POSIXct("1900-01-01"), as.POSIXct("2019-11-15"), 1.81, # min
  #  "HW",  as.POSIXct("2019-11-15"), as.POSIXct("2020-04-01"), 1.80,
  #  "HW",  as.POSIXct("2020-04-01"), as.POSIXct("2100-01-01"), 1.84, # max
  #  "SW",  as.POSIXct("1900-01-01"), as.POSIXct("2020-04-01"), 1.84, # min
  #  "SW",  as.POSIXct("2020-04-01"), as.POSIXct("2100-01-01"), 1.88 # max
  #)

  #ot <- ot |>
  #  left_join(calibration, by = "Site") |>
  #  filter(DateTime >= start & DateTime < end) |>
  #  dplyr::mutate(SD = (coef - TCDT_corr) * 100) |>
  #  dplyr::select(-start, -end, -coef)
  #rm(calibration)

  # make values that were below zero or outside of period of snowpack
  # occurrence (determined by Ed Lindsey and OTHS students
  # see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
  ot <- ot |>
    dplyr::mutate(
      SnowDepth = dplyr::case_when(
        Site == "HW" & DateTime > as.POSIXct("2019-04-16") & DateTime < as.POSIXct("2019-11-13") ~ 0,
        Site == "HW" & DateTime > as.POSIXct("2020-04-15") & DateTime < as.POSIXct("2020-12-07") ~ 0,
        Site == "HW" & DateTime > as.POSIXct("2021-03-28") & DateTime < as.POSIXct("2021-11-28") ~ 0,
        Site == "HW" & DateTime > as.POSIXct("2022-03-30") ~ 0,
        Site == "SW" & DateTime > as.POSIXct("2019-04-17") & DateTime < as.POSIXct("2019-11-13") ~ 0,
        Site == "SW" & DateTime > as.POSIXct("2020-04-15") & DateTime < as.POSIXct("2020-12-07") ~ 0,
        Site == "SW" & DateTime > as.POSIXct("2021-03-22") & DateTime < as.POSIXct("2021-11-28") ~ 0,
        Site == "SW" & DateTime > as.POSIXct("2022-03-30") ~ dplyr::if_else(SD < 0, 0, SD),
        .default = dplyr::if_else(SD < 0, 0, SD)
      )
    )

  # adjust
  ot <- ot |>
    dplyr::mutate(
      #subtract snow sensor height from ARD_DT_corr to get snow depth#
      #height is 1920 in HW and 1937 in SW
      #divide by 10 to get units in cm
      ARD_SD = dplyr::if_else(
        Site == "HW",
        (1920 - ARD_DT_corr) / 10,
        (1937 - ARD_DT_corr) / 10
      ),
      #make values that were below zero or outside of period of snowpack
      #occurrence (determined by Ed Lindsey and OTHS students
      #see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
      ARD_SnowDepth = dplyr::case_when(
        DateTime > as.POSIXct("2021-03-22") & DateTime < as.POSIXct("2021-11-28") ~ 0,
        DateTime > as.POSIXct("2022-03-30") ~ 0,
        ARD_SD < 0 ~ 0,
        ARD_SD > quantile(SD, 0.99, na.rm = TRUE) ~ NA_real_,
        .default = ARD_SD
      ),
      #convert VWC data collected by open source sensor to range dplyr::between 0 and 1
      ARD_VWC = ARD_VWC_corr / 100,
      #correct electrical conductivity measurements for
      #temperature, which will provide specific conductance
      SpeCon_5 = EC_5_corr / (1 + 0.02 * (TSoil_5_corr - 25)),
      SpeCon_25 = EC_25_corr / (1 + 0.02 * (TSoil_25_corr - 25)),
    )

  # rename columns
  ot <- ot |>
    dplyr::select(all_of(import_names())) |>
    dplyr::rename_with(~ import_renames(), .cols = all_of(import_names()))

  # add ARD flag instead of seperate columns
  non_sensor <- c("Site", "DateTime", "date", "Year", "Month", "Day", "Hour")
  os_sensor <- colnames(ot)[startsWith(colnames(ot), "ARD_")] # ARD
  prop_sensor <- setdiff(colnames(ot), c(non_sensor, os_sensor))
  ot_ARD <- ot |>
    dplyr::select(
      dplyr::all_of(non_sensor),
      dplyr::starts_with("ARD_")
    ) |>
    dplyr::rename_with(~ stringr::str_remove(., "ARD_")) |>
    dplyr::mutate(sensor = "os")

  ot <- ot |>
    dplyr::select(
      dplyr::all_of(non_sensor),
      dplyr::all_of(prop_sensor)
    ) |>
    dplyr::mutate(sensor = "prop")

  ot <- dplyr::bind_rows(ot, ot_ARD)
  rm(non_sensor, os_sensor, prop_sensor, ot_ARD)

  #TODO reimplement this
  #goog = read_sheet(
  #  "https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705"
  #)

  google_df <- readr::read_csv("~/Desktop/field_data_responses.csv") |>
    dplyr::mutate(
      DateTime = round(as.POSIXct(Timestamp, format = "%m/%d/%Y %H:%M", tz = "EST"), "hours"), # round time to nearest hour
      date = lubridate::as_date(DateTime, tz = "EST"),
      site = Neighborhood,
      sensor = "google",
      fdep = `Average Whole Site FROST depth (mm)` / 10,
      swe = `SWE (inches of water after snow sample melts)` * 25,
      SnowDepth = `Average Whole Site SNOW depth (cm)`
    ) |>
    dplyr::select(DateTime, date, site, sensor, fdep, swe, SnowDepth)

  combined <- ot |>
    dplyr::full_join(
      google_df,
      by = c(
        "DateTime" = "DateTime",
        "Site" = "site",
        "date" = "date",
        "sensor" = "sensor",
        "SnowDepth" = "SnowDepth"
      )
    )
  rm(ot, google_df)

  #TODO change to updating files when available
  hwpheno = readr::read_csv("http://hbrsensor.sr.unh.edu/data/snownet/shiny_othshw.csv") |>
    dplyr::mutate(
      site = "HW",
      DateTime = lubridate::as_date(date, tz = "EST")
    ) |>
    dplyr::select(DateTime, midday_gcc, site)

  swpheno = readr::read_csv("http://hbrsensor.sr.unh.edu/data/snownet/shiny_othssw.csv") |>
    dplyr::mutate(
      site = "SW",
      DateTime = lubridate::as_date(date, tz = "EST")
    ) |>
    dplyr::select(DateTime, midday_gcc, site)

  pheno = dplyr::bind_rows(hwpheno, swpheno)
  rm(hwpheno, swpheno)

  #write_csv(ot, "~/Desktop/ot_old.csv")
  #ot_1 <- readr::read_csv("~/Desktop/ot_old.csv") |> dplyr::mutate(Site = as.factor(Site))
  #ot_2 <- readr::read_csv("~/Desktop/ot_oldest.csv") |> dplyr::select(-1) |> dplyr::mutate(Site = as.factor(Site))
  #full_explore_output(ot_1, "~/Desktop/ot_1.pdf")
  #full_explore_output(ot_2, "~/Desktop/ot_2.pdf")

  return(list(combined, pheno))
}
