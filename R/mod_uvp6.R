#' uvp6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#' @import ggplot2 ggiraph
mod_uvp6_ui <- function(id){
  ns <- NS(id)

  tagList(
        girafeOutput(ns("plot_parking_uvp"), height = "700px", width = "100%")
  )

}

#' uvp6 Server Functions
#'
#' @noRd
mod_uvp6_server <- function(id, user_float, float_colour_zone){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Sys.setlocale("LC_TIME", "en_GB.utf8")

    # particle size classe
    lpm_classes <- c('NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
                     'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
                     'NP_Size_1630','NP_Size_2050')

    # facets for plot
    facet_all <- c(`102` = '102 - 128 µm',
                   `128` = '128 - 161 µm',
                   `161` = '161 - 203 µm',
                   `203` = '203 - 256 µm',
                   `256` = '256 - 323 µm',
                   `323` = '323 - 406 µm',
                   `406` = '406 - 512 µm',
                   `512` = '512 - 645 µm',
                   `645` = '645 - 813 µm',
                   `813` = '0.81 - 1.02 mm',
                   `1020` = '1.02 - 1.29 mm',
                   `1290` = '1.29 - 1.63 mm',
                   `1630` = '1.63 - 2.05 mm',
                   `2050` = '2.05 - 2.50 mm')

    # create waiter
    w <- waiter::Waiter$new(ns("plot_parking_uvp"), color = 'white', html = spin_dots())

    # UVP6 data to plot
    particle_data <- reactive({
      req(user_float$wmo())
      w$show()
      # compute daily mean particle concentration for all floats given in input
      tmp <- purrr::map_dfr(user_float$wmo(), compute_daily_mean_part_conc, .progress = TRUE)
      # add colour scheme
      tmp <- merge(tmp, float_colour_zone)
    })

    # render plot
    output$plot_parking_uvp <- renderGirafe({
      shiny::validate(shiny::need(nrow(particle_data()) > 0, message = "Error retrieving UVP6 data."))
      p <- particle_data() %>% dplyr::filter(park_depth == user_float$park_depth()) %>% ggplot() +
        geom_point(aes(juld, mean_conc, colour = colour)) +
        scale_colour_identity() +
        theme_bw() + labs(x = 'Date', y = 'Particle abundance (#/L)') +
        scale_y_continuous(trans = 'log10') +
        facet_wrap(~size, scales = 'free_y', labeller = as_labeller(facet_all)) +
        theme(legend.position = "none") +
        theme(text = element_text(size = 10)) +
        scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
        theme(axis.text.x = element_text(angle=45, hjust = 1))
      p <- ggiraph::girafe(ggobj = p, width_svg = 8)
    })
  })
}
