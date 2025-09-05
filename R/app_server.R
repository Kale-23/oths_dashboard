#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  both <- import_data()
  bound <- both[[1]]
  pheno <- both[[2]]
  # Your application server logic
  mod_global_server("global_1", bound = bound, pheno = pheno)
}
