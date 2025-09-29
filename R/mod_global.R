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
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 600,
        title = "Old Town Ecological Observatory",
        # intro text
        shiny::p(
          "Visualize and download data and graphs from the Old Town Ecological Observatory, Old Town, Maine. The observatory continuously monitors near-surface climate conditions in two neighboring forest stands, beech and hemlock"
        ),
        # date input
        shiny::uiOutput(ns("date_ui")),
        # sensor input
        shiny::checkboxGroupInput(
          ns("sensor_selector"),
          "Select Sensors to Include",
          choices = c(
            "Open Source (Arduino)" = "os",
            "Proprietary" = "prop",
            "Google Sheets" = "google"
          ),
          selected = c("os", "prop", "google"),
        ),
        # site input
        shiny::radioButtons(
          ns("site_selector"),
          "Select Site to Include",
          choices = c(
            "Both" = "both",
            "Beech" = "hw",
            "Hemlock" = "sw"
          ),
          selected = "both"
        ),
        # data to plot
        shiny::checkboxGroupInput(
          ns("plot_selector"),
          "Select Variables to Plot",
          choices = c(
            "Air Temperature" = "AirT", # AirT
            "Relative Humidity" = "RH", # RH
            "Snow Depth" = "SnowDepth", # SnowDepth
            #"Soil Temperature" = "sdep", # sdep
            "Snow Water Equivalent" = "swe", # swe
            "Frost Depth" = "fdep", # fdep
            "Soil Temperature" = "TSoil", # TSoil_5, TSoil_25, TSoil
            "Soil Moisture" = "VWC", # VWC_5, VWC_25, VWC
            "Soil Specific Conductance" = "SpeCon", # SpeCon_5, SpeCon_25
            "Phenology Greenness Index" = "gcc" # midday_gcc
          )
        ),
        p(
          "This research is supported by the National Science Foundation MacroSystems Biology
                Grant #18027726"
        )
      ),

      shiny::uiOutput(ns("AirT_plot")),

      #shiny::conditionalPanel(
      #  condition = "input.plot_selector.includes('AirT')",
      #  dygraphs::dygraphOutput(ns("AirT_plot")),
      #),
      #shiny::conditionalPanel(
      #  condition = "input.plot_selector.includes('RH')",
      #  dygraphs::dygraphOutput(ns("RH_plot")),
      #),
      #shiny::conditionalPanel(
      #  condition = "input.plot_selector.includes('SnowDepth')",
      #  dygraphs::dygraphOutput(ns("SnowDepth_plot")),
      #),
      #shiny::conditionalPanel(
      #  condition = "input.plot_selector.includes('swe')",
      #  dygraphs::dygraphOutput(ns("swe_plot")),
      #),
      #shiny::conditionalPanel(
      #  condition = "input.plot_selector.includes('gcc')",
      #  dygraphs::dygraphOutput(ns("gcc_plot")),
      #),

      DT::dataTableOutput(ns("bound_dt")),
      DT::dataTableOutput(ns("pheno_dt")),
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
        "Select Date Range",
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
            TRUE
          }
        )
    })

    filtered_pheno <- shiny::reactive({
      req(input$date_selector)
      pheno |>
        dplyr::filter(
          DateTime >= input$date_selector[1] &
            DateTime <= input$date_selector[2]
        )
    })

    output$AirT_plot <- shiny::renderUI({
      plotly::plotlyOutput(
        plotly::renderPlotly(
          single_col_plot(
            filtered_bound(),
            "AirT"
          )
        )
      )
    })

    #output$AirT_plot <- dygraphs::renderDygraph(single_col_plot(
    #   filtered_bound(),
    #   "AirT"
    # ))

    #output$RH_plot <- dygraphs::renderDygraph(single_col_plot(
    #  filtered_bound(),
    #  "RH"
    #))

    #output$SnowDepth_plot <- dygraphs::renderDygraph(single_col_plot(
    #  filtered_bound(),
    #  "SnowDepth"
    #))

    #output$swe_plot <- dygraphs::renderDygraph(single_col_plot(
    #  filtered_bound(),
    #  "swe"
    #))

    #output$gcc_plot <- dygraphs::renderDygraph(single_col_plot(
    #  filtered_pheno(),
    #  "midday_gcc"
    #))

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
