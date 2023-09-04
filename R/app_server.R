#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  selected_float <- mod_select_float_server("sidebar")
  float_colour_zone <- mod_float_map_server("float_map")
  mod_uvp6_server("uvp6", selected_float, float_colour_zone)
  mod_ost_server("ost", selected_float, float_colour_zone)

}
