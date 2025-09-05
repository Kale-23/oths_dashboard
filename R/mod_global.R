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
        shiny::radioButtons(
          ns("sensor_selector"),
          "Select Sensors to Include",
          choices = c(
            "Open Source (Arduino)" = "os",
            "Proprietary" = "prop",
            "Both" = "both"
          ),
          selected = "both"
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
        )
      ),
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
        start = min(bound$date, pheno$date),
        min = min(bound$date, pheno$date),
        end = max(bound$date, pheno$date),
        max = max(bound$date, pheno$date),
      )
    })

    # EVERYTHING BELOW SHOULD USE THE FILERED DATAFRAMES
    filtered_bound <- shiny::reactive({
      req(input$date_selector)
      req(input$sensor_selector)

      # sensor setup
      #fmt: skip
      non_sensor <- c("Site", "DateTime", "date", "Year", "Month", "Day", "Hour", "fdep", "swe", "sdep")
      os_sensor <- colnames(bound)[startsWith(colnames(bound), "ARD_")]
      prop_sensor <- setdiff(colnames(bound), c(non_sensor, os_sensor))

      print(os_sensor)
      print(prop_sensor)

      bound |>
        dplyr::filter(
          # date filter
          date >= input$date_selector[1] &
            date <= input$date_selector[2]
        ) |>
        # sensor filter
        dplyr::select(
          if (input$sensor_selector == "os") {
            c(non_sensor, os_sensor)
          } else if (input$sensor_selector == "prop") {
            c(non_sensor, prop_sensor)
          } else {
            dplyr::everything()
          }
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
          date >= input$date_selector[1] &
            date <= input$date_selector[2]
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
