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
.selectize-input .item[data-value='Labrador Sea'] {
  background-color: #E41A1C !important;
}
.selectize-input .item[data-value='Arabian Sea'] {
  background-color: #377EB8 !important;
}
.selectize-input .item[data-value='Guinea Dome']{
  background-color: #4DAF4A !important;
}
.selectize-input .item[data-value='Apero mission'] {
  background-color: #984EA3 !important;
}
.selectize-input .item[data-value='West Kerguelen'] {
  background-color: #FF7F00 !important;
}
.selectize-input .item[data-value='East Kerguelen'] {
  background-color: #FFFF33 !important;
}
.selectize-input .item[data-value='Tropical Indian Ocean'] {
  background-color: #A65628 !important;
}
.selectize-input .item[data-value='South Pacific Gyre'] {
  background-color: #F781BF !important;
}
.selectize-input .item[data-value='California Current'] {
  background-color: #999999 !important;
}
.selectize-input .item[data-value='Nordic Seas'] {
  background-color: #125112 !important;
  color: #FFFFFF !important;
}
.selectize-input .item[data-value='North Pacific Gyre'] {
  background-color: #91C5F0 !important;
}
"

  tagList(
    tags$head(
      tags$style(HTML(css))
    ),
    selectInput(inputId = ns("wmo"),
                label = "Float WMO",
                choices = unique(RefineParking::wmo_list$WMO),
                selectize = TRUE,
                multiple = TRUE,
    ),
    selectInput(inputId = ns("region"),
                label = "Region",
                choices = c("Labrador Sea", "Arabian Sea", "Guinea Dome", "Apero mission", "West Kerguelen", "East Kerguelen",
                            "Tropical Indian Ocean", "South Pacific Gyre", "California Current", "Nordic Seas", "North Pacific Gyre"),
                selectize = TRUE,
                multiple = TRUE,
    ),
    checkboxInput(inputId = ns("region_colour"),
                  label = "Colour by region",
                  value = F,
    ),
    selectInput(inputId = ns("park_depth"),
                label = "Parking depth",
                choices = c('200 m', '500 m', '1000 m'),
                multiple = TRUE,
                selected = '200 m',
    ),
    selectInput(inputId = ns("size_class"),
                label = "UVP6 size class",
                choices = c('102 - 128 µm' = 'NP_Size_102',
                            '128 - 161 µm' = 'NP_Size_128',
                            '161 - 203 µm' = 'NP_Size_161',
                            '203 - 256 µm' = 'NP_Size_203',
                            '256 - 323 µm' = 'NP_Size_256',
                            '323 - 406 µm' = 'NP_Size_323',
                            '406 - 512 µm' = 'NP_Size_406',
                            '512 - 645 µm' = 'NP_Size_512',
                            '645 - 813 µm' = 'NP_Size_645',
                            '0.81 - 1.02 mm' = 'NP_Size_813',
                            '1.02 - 1.29 mm' = 'NP_Size_1020',
                            '1.29 - 1.63 mm' = 'NP_Size_1290',
                            '1.63 - 2.05 mm' = 'NP_Size_1630',
                            '2.05 - 2.50 mm' = 'NP_Size_2050'),
                selected = NULL,
                multiple = TRUE,
                selectize = TRUE
    )
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
      region = reactive(input$region),
      region_colour = reactive(input$region_colour),
      park_depth = reactive(input$park_depth),
      size_class = reactive(input$size_class)
    )
  })
}
