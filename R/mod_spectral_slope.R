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
    actionButton(inputId = ns("compute_spectral_slope"), label = "Compute spectral slope"),
    girafeOutput(ns("plot_spectral_slope"), height = "700px", width = "100%")
  )
}

#' spectral_slope Server Functions
#'
#' @noRd
mod_spectral_slope_server <- function(id, user_float, float_colour_zone){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create waiter
    w <- waiter::Waiter$new(ns("plot_spectral_slope"), color = 'white', html = spin_dots())

    # slope data to plot
    slope_data <- eventReactive(input$compute_spectral_slope, {
      req(user_float$wmo())
      w$show()
      # compute daily meaan spectral slope for all floats given in input
      tmp <- purrr::map_dfr(user_float$wmo(), compute_spectral_slope, .progress = TRUE)
      # add colour scheme
      tmp <- merge(tmp, float_colour_zone)
    })

    # render plot
    output$plot_spectral_slope <- renderGirafe({
      shiny::validate(shiny::need(nrow(slope_data()) > 0, message = "Error computing spectral slope."))
      p <- slope_data() %>% dplyr::filter(park_depth == user_float$park_depth()) %>% ggplot() +
        geom_point(aes(x = date, y = mean_slope, colour = colour)) +
        scale_colour_identity() +
        theme_bw() + labs(x = 'Date', y = 'Daily average slope') +
        theme(legend.position = "none") +
        theme(text = element_text(size = 10)) +
        scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
        theme(axis.text.x = element_text(angle=45, hjust = 1))
      p <- ggiraph::girafe(ggobj = p, width_svg = 8)
    })

  })
}

## To be copied in the UI
# mod_spectral_slope_ui("spectral_slope_1")

## To be copied in the server
# mod_spectral_slope_server("spectral_slope_1")
