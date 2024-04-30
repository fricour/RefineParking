#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny httr2
#' @noRd
app_server <- function(input, output, session) {
  Sys.setlocale("LC_TIME", "en_GB.utf8")

  # Get paths to BGC-Argo main (CORE) and auxiliary (AUX) files
  path_to_GDAC_CORE_data <- golem::get_golem_options("path_to_GDAC_CORE_data")
  path_to_GDAC_AUX_data <- golem::get_golem_options("path_to_GDAC_AUX_data")

  # path to argo synthetic profile index file (in order to get the latitude/longitude for each profile)
  index_data <- request("https://data-argo.ifremer.fr/argo_synthetic-profile_index.txt") %>%
    req_perform() %>%
    resp_body_string()

  # invoke modules
  selected_float <- mod_select_float_server("sidebar")
  float_colour_zone <- mod_float_map_server("float_map", index_data)
  mod_uvp6_server("uvp6", selected_float, float_colour_zone, path_to_GDAC_AUX_data)
  mod_ost_server("ost", selected_float, float_colour_zone, path_to_GDAC_CORE_data)
  mod_spectral_slope_server("spectral_slope", selected_float, float_colour_zone, path_to_GDAC_AUX_data)
}
