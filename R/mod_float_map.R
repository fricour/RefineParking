#' float_map UI Function
#'
#' @description Trajectories of floats equipped with both the Underwater Vision Profile 6 and the transmissometer used as an Optical Sediment Trap (OST)
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
mod_float_map_ui <- function(id){
  ns <- NS(id)

  tagList(
        leafletOutput(ns("map"))
  )

}

#' float_map Server Functions
#'
#' @noRd
mod_float_map_server <- function(id, index_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    w <- waiter::Waiter$new(ns("map"), color = 'white', html = spin_dots())
    w$show()

    bio_index <- tibble::tibble(data.table::fread(index_data, skip=8))

    # format table to keep wmo, profile, date, latitude and longitude
    bio_index <- bio_index %>%
      dplyr::mutate(wmo = purrr::map_chr(.x = bio_index$file, .f = function(x) unlist(stringr::str_split(x, '/'))[2])) %>%
      dplyr::select(wmo, date, latitude, longitude) %>%
      dplyr::filter(wmo %in% unique(RefineParking::wmo_list$WMO)) %>%
      dplyr::mutate(zone = NA) %>%
      dplyr::mutate(colour = NA) %>%
      dplyr::filter(!is.na(latitude)) %>%
      # add name for each geographic zone where floats are deployed
      dplyr::mutate(zone = dplyr::case_when(
        wmo %in% c(6904240, 6904241, 1902578, 4903634) ~ 'Labrador Sea',
        wmo %in% c(4903660, 6990514) ~ 'Arabian Sea',
        wmo %in% c(3902498, 1902601) ~ 'Guinea Dome',
        wmo %in% c(1902637, 4903740, 4903739) ~ 'Apero mission',
        wmo %in% c(2903787, 4903657) ~ 'West Kerguelen',
        wmo %in% c(1902593, 4903658) ~ 'East Kerguelen',
        wmo %in% c(5906970, 3902473, 6990503, 3902471) ~ 'Tropical Indian Ocean',
        wmo %in% c(2903783) ~ 'South Pacific Gyre',
        wmo %in% c(6903093, 6903094) ~'California Current',
        wmo %in% c(7901028, 2903794) ~ 'Nordic Seas',
        wmo %in% c(1902685) ~ 'North Pacific Gyre',
        .default = NA
      )) %>%
      dplyr::mutate(colour = dplyr::case_when(
        zone == 'Labrador Sea' ~ '#E41A1C',
        zone == 'Arabian Sea' ~ '#377EB8',
        zone == 'Guinea Dome' ~ '#4DAF4A',
        zone == 'Apero mission' ~ '#984EA3',
        zone == 'West Kerguelen' ~ '#FF7F00',
        zone == 'East Kerguelen' ~ '#FFFF33',
        zone == 'Tropical Indian Ocean' ~ '#A65628',
        zone == 'South Pacific Gyre' ~ '#F781BF',
        zone == 'California Current' ~ '#999999',
        zone == 'Nordic Seas' ~ '#125112',
        zone == 'North Pacific Gyre' ~ '#91C5F0',
        .default = NA
      ))

    # create polyline objects for leafletgl
    traj_list <- bio_index %>%
      dplyr::group_by(wmo) %>%
      dplyr::arrange(date) %>%
      dplyr::summarize(geometry = sf::st_sfc(sf::st_linestring(cbind(longitude, latitude))))

    # Convert the summarized data to an sf dataframe with spatial lines
    lines_sf <- sf::st_as_sf(traj_list) %>%
      # add name for each geographic zone where floats are deployed
      dplyr::mutate(zone = dplyr::case_when(
        wmo %in% c(6904240,6904241,1902578,4903634) ~ 'Labrador Sea',
        wmo %in% c(4903660, 6990514) ~ 'Arabian Sea',
        wmo %in% c(3902498, 1902601) ~ 'Guinea Dome',
        wmo %in% c(1902637, 4903740, 4903739) ~ 'Apero mission',
        wmo %in% c(2903787, 4903657) ~ 'West Kerguelen',
        wmo %in% c(1902593, 4903658) ~ 'East Kerguelen',
        wmo %in% c(5906970, 3902473, 6990503, 3902471) ~ 'Tropical Indian Ocean',
        wmo %in% c(2903783) ~ 'South Pacific Gyre',
        wmo %in% c(6903093, 6903094) ~'California Current',
        wmo %in% c(7901028, 2903794) ~ 'Nordic Seas',
        wmo %in% c(1902685) ~ 'North Pacific Gyre',
        .default = NA
      )) %>%
      dplyr::mutate(colour = dplyr::case_when(
        zone == 'Labrador Sea' ~ '#E41A1C',
        zone == 'Arabian Sea' ~ '#377EB8',
        zone == 'Guinea Dome' ~ '#4DAF4A',
        zone == 'Apero mission' ~ '#984EA3',
        zone == 'West Kerguelen' ~ '#FF7F00',
        zone == 'East Kerguelen' ~ '#FFFF33',
        zone == 'Tropical Indian Ocean' ~ '#A65628',
        zone == 'South Pacific Gyre' ~ '#F781BF',
        zone == 'California Current' ~ '#999999',
        zone == 'Nordic Seas' ~ '#125112',
        zone == 'North Pacific Gyre' ~ '#91C5F0',
        .default = NA
      ))

    # plot floats
    output$map <- renderLeaflet({
      shiny::validate(shiny::need(nrow(bio_index > 0), message = "Wait for the actualization of the map"))
      leaflet() %>%
        addTiles() %>%
        #addCircleMarkers(data = bio_index, lng = ~longitude, lat = ~latitude, popup = ~wmo, color = ~colour) %>%
        #leafgl::addGlPolylines(data = lines_sf, color = ~colour, opacity = 1)
        addPolylines(data = lines_sf, color = ~colour, opacity = 1, popup = ~htmltools::htmlEscape(wmo), label = ~wmo,
                     highlightOptions = highlightOptions(color = "white", weight = 5,
                                                         bringToFront = TRUE, sendToBack = TRUE))
    })

    float_colour_zone <- bio_index %>% dplyr::select(wmo, zone, colour) %>% dplyr::distinct()
    return(float_colour_zone)

  })
}
