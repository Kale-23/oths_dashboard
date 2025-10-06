#' global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_global_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::span(
            shiny::h3(
              style = "display: inline; margin-right: 0.3em;",
              "Old Town Ecological Observatory Dashboard"
            ),
            bslib::tooltip(
              bsicons::bs_icon("info-circle"),
              # intro text
              "Visualize and download data and graphs from the Old Town Ecological Observatory, Old Town, Maine. The observatory continuously monitors near-surface climate conditions in two neighboring forest stands, beech and hemlock",
              placement = "auto"
            ),
          ),
          bslib::input_dark_mode()
        ),
      ),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 500,
          shiny::div(
            # date input
            shiny::uiOutput(ns("date_ui")),
            # sensor input
            shiny::checkboxGroupInput(
              ns("sensor_selector"),
              shiny::span(
                "Select Sensors to Include",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Not all sensors collect all variables. If no data appears for a variable, try selecting a different sensor.",
                  placement = "right"
                )
              ),
              choices = c(
                "Arduino (Open Source)" = "os",
                "Research Grade" = "prop",
                "Volunteer (Google Sheets)" = "google"
              ),
              selected = c("os", "prop", "google"),
            ),
            # site input
            shiny::radioButtons(
              ns("site_selector"),
              shiny::span(
                "Select Site to Include",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Beech (HW) and Hemlock (SW)",
                  placement = "right"
                )
              ),
              choices = c(
                "Both" = "both",
                "Beech" = "hw",
                "Hemlock" = "sw"
              ),
              selected = "both"
            ),
            shiny::radioButtons(
              ns("aggregate_selector"),
              shiny::span(
                "Aggregate Data by",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Will take the mean of all data points within the selected time period, floored. This greatly speeds up plotting for long time series. Note that this will not affect the data tables below, which will show the full date and time of each observation.",
                  placement = "right"
                )
              ),
              choices = c(
                "Do Not Aggregate" = "none",
                "Hour" = "hour",
                "Day" = "day",
                "Week" = "week",
                "Month" = "month"
              ),
              selected = "week"
            ),
            # data to plot
            shiny::checkboxGroupInput(
              ns("plot_selector"),
              shiny::span(
                "Select Variables to Plot",
                bslib::tooltip(
                  bsicons::bs_icon("info-circle"),
                  "You can select multiple variables to plot. Each variable will be shown in its own plot. If there are multiple depths for a variable, each depth will be shown as a separate line within the same plot.",
                  placement = "right"
                )
              ),
              choices = c(
                "Air Temperature" = "AirT", # AirT
                "Relative Humidity" = "RH", # RH
                "Snow Depth" = "SnowDepth", # SnowDepth
                "Snow Water Equivalent" = "swe", # swe
                "Frost Depth" = "fdep", # fdep
                "Soil Temperature" = "TSoil", # TSoil_5, TSoil_25, TSoil
                "Soil Moisture" = "VWC", # VWC_5, VWC_25, VWC
                "Soil Specific Conductance" = "SpeCon", # SpeCon_5, SpeCon_25
                "Phenology Greenness Index" = "midday_gcc" # midday_gcc
              ),
              selected = c("AirT")
            ),
            p(
              "This research is supported by the National Science Foundation MacroSystems Biology
                Grant #18027726"
            )
          )
        ), # end sidebar

        shiny::uiOutput(ns("bound_plots")),
        shiny::uiOutput(ns("pheno_plots")),
        bslib::card(
          full_screen = TRUE, # can expand to fit screen
          bslib::card_header(
            "Bound Sensor Data Table"
          ),
          DT::dataTableOutput(ns("bound_dt")),
          shiny::downloadButton(ns("download_bound"), "Download Filtered Bound Data")
        ),
        bslib::card(
          full_screen = TRUE, # can expand to fit screen
          bslib::card_header(
            "Phenocam Data Table"
          ),
          DT::dataTableOutput(ns("pheno_dt")),
          shiny::downloadButton(ns("download_pheno"), "Download Filtered Phenocam Data")
        )
      )
    )
  )
}

#' global Server Functions
#'
#' @noRd
mod_global_server <- function(id, bound, pheno) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # date range input
    output$date_ui <- shiny::renderUI({
      shiny::dateRangeInput(
        ns("date_selector"),
        shiny::span(
          "Select Date Range",
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Date range will control all plots and data tables. You can control each plot's date range individually through the plotly toolbar.",
            placement = "right"
          )
        ),
        start = min(bound$date, pheno$DateTime),
        min = min(bound$date, pheno$DateTime),
        end = max(bound$date, pheno$DateTime),
        max = max(bound$date, pheno$DateTime),
      )
    })

    # EVERYTHING BELOW SHOULD USE THE FILERED DATAFRAMES
    filtered_bound <- shiny::reactive({
      req(input$date_selector)
      req(input$sensor_selector)
      req(input$site_selector)
      req(input$plot_selector)

      # sensor setup
      bound |>
        dplyr::filter(
          # date filter
          date >= input$date_selector[1] &
            date <= input$date_selector[2],
          # sensor filter
          sensor %in% input$sensor_selector
        ) |>
        # site filter
        dplyr::filter(
          if (input$site_selector == "hw") {
            Site == "HW"
          } else if (input$site_selector == "sw") {
            Site == "SW"
          } else {
            TRUE # no filter
          }
        ) |>
        # site filter
        dplyr::select(c("Site", "DateTime", "sensor", dplyr::matches(input$plot_selector)))
      # aggregate filter not done here so DTs show full dates
    })

    output$download_bound <- shiny::downloadHandler(
      filename = function() {
        paste("oths_bound_filtered_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(filtered_bound())
        readr::write_csv(filtered_bound(), file)
      }
    )

    filtered_pheno <- shiny::reactive({
      req(input$date_selector)
      pheno |>
        # date filter
        dplyr::filter(
          DateTime >= input$date_selector[1] &
            DateTime <= input$date_selector[2]
        ) |>
        # column filter
        dplyr::select(c("site", "DateTime", dplyr::matches(input$plot_selector)))
    })

    output$download_pheno <- shiny::downloadHandler(
      filename = function() {
        paste("oths_phenocam_filtered_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(filtered_pheno())
        readr::write_csv(filtered_pheno(), file)
      }
    )

    bound_plot_data <- shiny::reactive({
      req(input$plot_selector)
      req(input$aggregate_selector)
      req(filtered_bound())

      # do aggregation here so that the DTs show full dates
      aggregated_data <- filtered_bound() |>
        dplyr::mutate(
          DateTime = dplyr::case_when(
            input$aggregate_selector == "hour" ~ lubridate::floor_date(DateTime, unit = "hour"),
            input$aggregate_selector == "day" ~ lubridate::floor_date(DateTime, unit = "day"),
            input$aggregate_selector == "week" ~ lubridate::floor_date(DateTime, unit = "week"),
            input$aggregate_selector == "month" ~ lubridate::floor_date(DateTime, unit = "month"),
            TRUE ~ DateTime
          )
        )

      purrr::map(input$plot_selector, function(col) {
        if (col != "midday_gcc") {
          df <- prep_plot(aggregated_data, col)
          list(data = df, col = col)
        }
      }) |>
        purrr::compact() # remove NULLs (ie gcc)
    })

    output$bound_plots <- shiny::renderUI({
      req(bound_plot_data())
      tagList(
        lapply(bound_plot_data(), function(df_col) {
          df <- df_col$data
          col <- df_col$col

          # handle multi col IDs (ie 5 and 25)

          plot_id <- paste0("plot_", col)

          # create plots
          output[[plot_id]] <- plotly::renderPlotly({
            plotly_timeseries(df, col)
          })

          # show plots within card for better formatting
          bslib::card(
            full_screen = TRUE, # can expand to fit screen
            bslib::card_header(
              plot_name_conversions()[[col]]
            ),
            plotly::plotlyOutput(ns(plot_id)) # actual plot output
          )
        })
      )
    })

    pheno_plot_data <- shiny::reactive({
      req(input$plot_selector)
      req(input$aggregate_selector)
      req(filtered_pheno())

      # do aggregation here so that the DTs show full dates
      aggregated_data <- filtered_pheno() |>
        dplyr::mutate(
          DateTime = dplyr::case_when(
            input$aggregate_selector == "week" ~ lubridate::floor_date(DateTime, unit = "week"),
            input$aggregate_selector == "month" ~ lubridate::floor_date(DateTime, unit = "month"),
            TRUE ~ DateTime
          )
        ) |>
        dplyr::rename(Site = site)

      purrr::map(input$plot_selector, function(col) {
        if (col == "midday_gcc") {
          df <- prep_plot(aggregated_data, col)
          list(data = df, col = col)
        }
      }) |>
        purrr::compact() # remove NULLs (ie != gcc)
    })

    output$pheno_plots <- shiny::renderUI({
      req(pheno_plot_data())
      tagList(
        lapply(pheno_plot_data(), function(df_col) {
          df <- df_col$data
          col <- df_col$col

          # handle multi col IDs (ie 5 and 25)

          plot_id <- paste0("bound_plot_", col)

          # create plots
          output[[plot_id]] <- plotly::renderPlotly({
            plotly_timeseries(df, col)
          })

          # show plots within card for better formatting
          bslib::card(
            full_screen = TRUE, # can expand to fit screen
            bslib::card_header(
              plot_name_conversions()[[col]]
            ),
            plotly::plotlyOutput(ns(plot_id)) # actual plot output
          )
        })
      )
    })

    output$bound_dt <- DT::renderDT({
      DT::datatable(
        filtered_bound(),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })

    output$pheno_dt <- DT::renderDT({
      DT::datatable(
        filtered_pheno(),
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
  })
}

## To be copied in the UI
# mod_global_ui("global_1")

## To be copied in the server
# mod_global_server("global_1")
