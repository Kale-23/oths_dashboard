#' import
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
import_bound <- function() {
  common = "~/Desktop/Soil_Work/OTHS\ Shiny\ App\ 2.0/"
  #common = "https://hbrsensor.sr.unh.edu/data/snownet/"

  # get column names
  col_names = colnames(read_csv(
    paste0(common, "OTHSHW_SOIL_MET.dat"),
    n_max = 0,
    skip = 1
  ))

  # current dynamic files
  hw <- import_individual(paste0(common, "OTHSHW_SOIL_MET.dat"), "HW")
  sw <- import_individual(paste0(common, "OTHSSW_SOIL_MET.dat"), "SW")

  # old pre ardiuno
  hw_old <- import_individual(paste0(common, "OTHSHW_SOIL_MET.dat.backup"), "HW")
  sw_old <- import_individual(paste0(common, "OTHSSW_SOIL_MET.dat.backup"), "SW")

  ot <- bind_rows(hw, hw_old, sw, sw_old)
  rm(hw, sw, hw_old, sw_old, col_names, common)
  ot <- ot |>
    mutate(
      Datetime = TIMESTAMP,
      Year = year(Datetime),
      Month = month(Datetime),
      Day = day(Datetime),
      Hour = hour(Datetime),
    )

  # quality control (TRUE = good, FALSE = bad)
  ot <- ot |>
    mutate(
      batt_flag = if_else(batt_volt_Min >= 12, TRUE, FALSE),
      # air temp
      airT_flag = if_else(
        between(AirT, -40, 60),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_airT_flag = if_else(
        between(ARD_AirT, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      # relative humidity
      RH_flag = if_else(
        between(RH, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_RH_flag = if_else(
        between(ARD_RH, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      # snow depth
      TCDT_flag = if_else(
        between(TCDT, 0.5, 10),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_DT_flag = if_else(
        between(ARD_DT, 50, 1940),
        TRUE,
        FALSE,
        missing = NA
      ),
      Q_flag = if_else(
        between(Q, 152, 210),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil temp
      TSoil_5_flag = if_else(
        between(TSoil_5, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      TSoil_25_flag = if_else(
        between(TSoil_25, -50, 70),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_SoilT_flag = if_else(
        between(ARD_SoilT, -40, 123.8),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil moisture
      VWC_5_flag = if_else(
        between(VWC_5, 0, 1),
        TRUE,
        FALSE,
        missing = NA
      ),
      VWC_25_flag = if_else(
        between(VWC_25, 0, 1),
        TRUE,
        FALSE,
        missing = NA
      ),
      ARD_VWC_flag = if_else(
        between(ARD_VWC_5, 0, 100),
        TRUE,
        FALSE,
        missing = NA
      ),
      # soil electrical conductivity
      EC_5_flag = if_else(
        between(EC_5, 0, 3),
        TRUE,
        FALSE,
        missing = NA
      ),
      EC_25_flag = if_else(
        between(EC_25, 0, 3),
        TRUE,
        FALSE,
        missing = NA
      )
    )

  # create corr columns
  ot <- ot |>
    mutate(
      AirT_corr = if_else(airT_flag, AirT, NA_real_),
      RH_corr = if_else(RH_flag, RH, NA_real_),
      TCDT_corr = if_else(TCDT_flag & Q_flag, TCDT, NA_real_),
      TSoil_5_corr = if_else(TSoil_5_flag, TSoil_5, NA_real_),
      TSoil_25_corr = if_else(TSoil_25_flag, TSoil_25, NA_real_),
      VWC_5_corr = if_else(VWC_5_flag, VWC_5, NA_real_),
      VWC_25_corr = if_else(VWC_25_flag, VWC_25, NA_real_),
      EC_5_corr = if_else(EC_5_flag, EC_5, NA_real_),
      EC_25_corr = if_else(EC_25_flag, EC_25, NA_real_),

      ARD_AirT_corr = if_else(ARD_airT_flag, ARD_AirT, NA_real_),
      ARD_RH_corr = if_else(ARD_RH_flag, ARD_RH, NA_real_),
      ARD_DT_corr = if_else(ARD_DT_flag, ARD_DT, NA_real_),
      ARD_SoilT_corr = if_else(ARD_SoilT_flag, ARD_SoilT, NA_real_),
      ARD_VWC_corr = if_else(ARD_VWC_flag, ARD_VWC_5, NA_real_)
    )

  # subtract snow sensor height from TCDT_corr to get snow depth
  # height varies over time as boom was adjusted
  # multiply by 100 to get units in cm
  ot <- ot |>
    mutate(
      SD = case_when(
        Site == "HW" & Datetime < as.POSIXct("2019-11-15") ~ (1.81 - TCDT_corr) * 100, # min
        Site == "HW" & Datetime >= as.POSIXct("2019-11-15") & Datetime < as.POSIXct("2020-04-01") ~
          (1.80 - TCDT_corr) * 100,
        Site == "HW" & Datetime >= as.POSIXct("2020-04-01") ~ (1.84 - TCDT_corr) * 100, # max
        Site == "SW" & Datetime < as.POSIXct("2020-04-01") ~ (1.84 - TCDT_corr) * 100, # min
        Site == "SW" & Datetime >= as.POSIXct("2020-04-01") ~ (1.88 - TCDT_corr) * 100 # max
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
  #  filter(Datetime >= start & Datetime < end) |>
  #  mutate(SD = (coef - TCDT_corr) * 100) |>
  #  select(-start, -end, -coef)
  #rm(calibration)

  # make values that were below zero or outside of period of snowpack
  # occurrence (determined by Ed Lindsey and OTHS students
  # see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
  ot <- ot |>
    mutate(
      SnowDepth = case_when(
        Site == "HW" & Datetime > as.POSIXct("2019-04-16") & Datetime < as.POSIXct("2019-11-13") ~ 0,
        Site == "HW" & Datetime > as.POSIXct("2020-04-15") & Datetime < as.POSIXct("2020-12-07") ~ 0,
        Site == "HW" & Datetime > as.POSIXct("2021-03-28") & Datetime < as.POSIXct("2021-11-28") ~ 0,
        Site == "HW" & Datetime > as.POSIXct("2022-03-30") ~ 0,
        Site == "SW" & Datetime > as.POSIXct("2019-04-17") & Datetime < as.POSIXct("2019-11-13") ~ 0,
        Site == "SW" & Datetime > as.POSIXct("2020-04-15") & Datetime < as.POSIXct("2020-12-07") ~ 0,
        Site == "SW" & Datetime > as.POSIXct("2021-03-22") & Datetime < as.POSIXct("2021-11-28") ~ 0,
        Site == "SW" & Datetime > as.POSIXct("2022-03-30") ~ if_else(SD < 0, 0, SD),
        .default = if_else(SD < 0, 0, SD)
      )
    )

  # adjust
  ot <- ot |>
    mutate(
      #subtract snow sensor height from ARD_DT_corr to get snow depth#
      #height is 1920 in HW and 1937 in SW
      #divide by 10 to get units in cm
      ARD_SD = if_else(
        Site == "HW",
        (1920 - ARD_DT_corr) / 10,
        (1937 - ARD_DT_corr) / 10
      ),
      #make values that were below zero or outside of period of snowpack
      #occurrence (determined by Ed Lindsey and OTHS students
      #see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
      ARD_SnowDepth = case_when(
        Datetime > as.POSIXct("2021-03-22") & Datetime < as.POSIXct("2021-11-28") ~ 0,
        Datetime < as.POSIXct("2021-11-28") ~ 0,
        Datetime > as.POSIXct("2022-03-30") ~ 0,
        ARD_SD < 0 ~ 0,
        ARD_SD > quantile(SD, 0.99, na.rm = TRUE) ~ NA_real_,
        .default = ARD_SD
      ),
      #convert VWC data collected by open source sensor to range between 0 and 1
      ARD_VWC = ARD_VWC_corr / 100,
      #correct electrical conductivity measurements for
      #temperature, which will provide specific conductance
      SpeCon_5 = EC_5_corr / (1 + 0.02 * (TSoil_5_corr - 25)),
      SpeCon_25 = EC_25_corr / (1 + 0.02 * (TSoil_25_corr - 25)),
    )

  ot <- ot |>
    select(all_of(import_names())) |>
    rename_with(~ import_renames(), .cols = all_of(import_names()))

  write_csv(ot, "~/Desktop/ot_old.csv")

  ot_1 <- read_csv("~/Desktop/ot_old.csv") |> mutate(Site = as.factor(Site))
  ot_2 <- read_csv("~/Desktop/ot_oldest.csv") |> select(-1) |> mutate(Site = as.factor(Site))

  full_explore_output(ot_1, "~/Desktop/ot_1.pdf")
  full_explore_output(ot_2, "~/Desktop/ot_2.pdf")
}
