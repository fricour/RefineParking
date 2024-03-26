#' spectral_slope UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph ggplot2
mod_spectral_slope_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("compute_spectral_slope"), label = "Compute spectral slope (only needed when adding or removing floats)"),
    girafeOutput(ns("plot_spectral_slope"), height = "700px", width = "100%")
  )
}

#' spectral_slope Server Functions
#'
#' @noRd
mod_spectral_slope_server <- function(id, user_float, float_colour_zone, path_to_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create waiter
    w <- waiter::Waiter$new(ns("plot_spectral_slope"), color = 'white', html = spin_dots())

    # slope data to plot
    slope_data <- eventReactive(input$compute_spectral_slope, {
      #req(user_float$wmo())
      w$show()
      # compute daily meaan spectral slope for all floats given in input
      if(is.null(user_float$region())){
        tmp <- purrr::map_dfr(user_float$wmo(), compute_spectral_slope, path_to_data = path_to_data, .progress = TRUE)
      }else{
        selected_float <- float_colour_zone %>%
          dplyr::filter(zone %in% user_float$region()) %>%
          dplyr::pull(wmo)
        tmp <- purrr::map_dfr(selected_float, compute_spectral_slope, path_to_data = path_to_data, .progress = TRUE)
      }
      # add colour scheme
      tmp <- merge(tmp, float_colour_zone)
      # add colour for parking depth levels
      tmp <- tmp %>% dplyr::mutate(colour_depth = dplyr::case_when(
        park_depth == '200 m' ~ '#fde725',
        park_depth == '500 m' ~ '#21908c',
        park_depth == '1000 m' ~ '#440154')
      )  %>%
        dplyr::mutate(park_depth = factor(park_depth, levels = c('200 m', '500 m', '1000 m')))
    })

    # render plot
    output$plot_spectral_slope <- renderGirafe({
      shiny::validate(shiny::need(nrow(slope_data()) > 0, message = "Error computing spectral slope."))

      if(user_float$region_colour()){
      p <- slope_data() %>% dplyr::filter(park_depth %in% user_float$park_depth()) %>% ggplot() +
        geom_point(aes(x = date, y = mean_slope, colour = zone, shape = park_depth)) +
        scale_colour_manual(values = c('Labrador Sea' = '#E41A1C',
                                       'Arabian Sea' = '#377EB8',
                                       'Guinea Dome' = '#4DAF4A',
                                       'Apero mission' = '#984EA3',
                                       'West Kerguelen' = '#FF7F00',
                                       'East Kerguelen' = '#FFFF33',
                                       'Tropical Indian Ocean' = '#A65628',
                                       'South Pacific Gyre' = '#F781BF',
                                       'California Current' = '#999999')) +
        theme_bw() + labs(x = 'Date', y = 'Daily average slope') +
        theme(legend.position = "top") +
        theme(text = element_text(size = 10)) +
        labs(shape = '', colour = '') +
        scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
        theme(axis.text.x = element_text(angle=45, hjust = 1))
      p <- ggiraph::girafe(ggobj = p, width_svg = 8)
      }else{
        p <- slope_data() %>% dplyr::filter(park_depth %in% user_float$park_depth()) %>% ggplot() +
          geom_point(aes(x = date, y = mean_slope, colour = factor(wmo), shape = park_depth)) +
          theme_bw() + labs(x = 'Date', y = 'Daily average slope') +
          theme(legend.position = "top") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1))
        p <- ggiraph::girafe(ggobj = p, width_svg = 8)
      }
    })

    # TODO : directly subset the data to compute the spectral slope depending on the selected input parking depth? That would save some time and have a better controlled behaviour. BUT in the
    # actual implementation, even if the behaviour is not perfect, it's still quite useful....

  })
}
