#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' admin Server Functions
#'
#' @noRd 
mod_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_admin_ui("admin_1")
    
## To be copied in the server
# mod_admin_server("admin_1")
