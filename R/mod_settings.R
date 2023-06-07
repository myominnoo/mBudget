#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
  	mod_category_ui(ns("category_1")),
  	mod_pay_account_ui(ns("pay_account_1"))
  )
}

#' settings Server Functions
#'
#' @noRd
mod_settings_server <- function(id, env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_category_server("category_1", env)
    mod_pay_account_server("pay_account_1", env)
  })
}

## To be copied in the UI
# mod_settings_ui("settings_1")

## To be copied in the server
# mod_settings_server("settings_1")
