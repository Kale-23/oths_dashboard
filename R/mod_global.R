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
      sidebar = shiny::div(),
      DT::dataTableOutput(ns("bound_dt")),
    )
  )
}

#' global Server Functions
#'
#' @noRd
mod_global_server <- function(id, bound, pheno) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$bound_dt <- DT::renderDT({
      DT::datatable(
        bound,
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
  })
}

## To be copied in the UI
# mod_global_ui("global_1")

## To be copied in the server
# mod_global_server("global_1")
