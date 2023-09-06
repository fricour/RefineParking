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

  # temporary css
  css <- "
.selectize-input .item[data-value='6904240'], .selectize-input .item[data-value='6904241'], .selectize-input .item[data-value='4903634'], .selectize-input .item[data-value='1902578'] {
  background-color: #E41A1C !important;
}
.selectize-input .item[data-value='1902601'], .selectize-input .item[data-value='3902498'] {
  background-color: #4DAF4A !important;
}
.selectize-input .item[data-value='1902593'], .selectize-input .item[data-value='4903658'] {
  background-color: #FFFF33 !important;
}
.selectize-input .item[data-value='1902637'], .selectize-input .item[data-value='4903739'], .selectize-input .item[data-value='4903740'] {
  background-color: #984EA3 !important;
}
.selectize-input .item[data-value='2903783'] {
  background-color: #F781BF !important;
}
.selectize-input .item[data-value='2903787'], .selectize-input .item[data-value='4903657'] {
  background-color: #FF7F00 !important;
}
.selectize-input .item[data-value='3902471'], .selectize-input .item[data-value='5906970'], .selectize-input .item[data-value='6990503'] {
  background-color: #A65628 !important;
}
.selectize-input .item[data-value='4903660'], .selectize-input .item[data-value='6990514'] {
  background-color: #377EB8 !important;
}
.selectize-input .item[data-value='6903093'], .selectize-input .item[data-value='6903094'] {
  background-color: #999999 !important;
}
"

  tagList(
    tags$head(
      tags$style(HTML(css))
    ),
    selectInput(inputId = ns("wmo"),
                label = "Float WMO",
                choices = unique(RefineParking::c_rover_calib$WMO),
                selectize = TRUE,
                multiple = TRUE,
    ),
    selectInput(inputId = ns("park_depth"),
                label = "Parking depth",
                choices = c('200 m', '500 m', '1000 m'))
    ,
    numericInput(inputId = ns("uvp_point_size"),
                 label = "UVP6 point size",
                 value = 1,
                 min = 0.1,
                 max= Inf)
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
      park_depth = reactive(input$park_depth),
      uvp_point_size = reactive(input$uvp_point_size)
    )
  })
}
