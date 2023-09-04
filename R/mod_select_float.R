#' select_float UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_float_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("wmo"),
                label = "Float WMO",
                choices = unique(RefineParking::c_rover_calib$WMO),
                multiple = TRUE,
    ),
    selectInput(inputId = ns("park_depth"),
                label = "Parking depth",
                choices = c('200 m', '500 m', '1000 m'))
  )
}

#' select_float Server Functions
#'
#' @noRd
mod_select_float_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # return selected WMO and profile number
    list(
      wmo = reactive(input$wmo),
      park_depth = reactive(input$park_depth)
    )
  })
}
