#' ost UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import waiter
mod_ost_ui <- function(id){
  ns <- NS(id)

  tagList(
    actionButton(inputId = ns("run_computation"), label = "Compute OST fluxes"),
    ggiraph::girafeOutput(ns("plot_parking_OST"), height = "700px")
  )
}

#' ost Server Functions
#'
#' @noRd
mod_ost_server <- function(id, user_float, float_colour_zone){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Sys.setlocale("LC_TIME", "en_GB.utf8")

    # create waiter
    w <- waiter::Waiter$new(ns("plot_parking_OST"), color = 'white', html = spin_dots())

    # OST data to plot
    ost_data <- eventReactive(input$run_computation, {
      req(user_float$wmo())
      w$show()
      # derive ost data
      tmp <- purrr::map_dfr(user_float$wmo(), extract_ost_data)
      # for plotting
      tmp <- tmp %>%
        dplyr::mutate(park_depth = factor(park_depth, levels = c('200 m', '500 m', '1000 m'))) %>%
        dplyr::mutate(min_time = as.Date(min_time))
      # add colour scheme
      tmp <- merge(tmp, float_colour_zone)
    })


    # render plot
    output$plot_parking_OST <- ggiraph::renderGirafe({
      shiny::validate(shiny::need(nrow(ost_data()) > 0, message = "Enter a valid WMO or a float that is equipped with the OST."))

      small_flux <- ost_data() %>% ggplot() +
        geom_point(aes(x = min_time, y = small_flux, colour = colour)) +
        scale_colour_identity() +
        facet_wrap(~park_depth, scales = 'free') +
        theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{small}$ (mg C m$^{-2}$ day$^{-1}$)')) +
        scale_y_continuous(trans = 'log10') +
        theme(legend.position = "none") +
        theme(text = element_text(size = 10)) +
        scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
        theme(axis.text.x = element_text(angle=45, hjust = 1))

      large_flux <- ost_data() %>% ggplot() +
        geom_point(aes(x = min_time, y = large_flux, colour = colour)) +
        scale_color_identity() +
        facet_wrap(~park_depth, scales = 'free') +
        theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{large}$ (mg C m$^{-2}$ day$^{-1}$)')) +
        scale_y_continuous(trans = 'log10') +
        theme(legend.position = "none") +
        theme(text = element_text(size = 10)) +
        scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
        theme(axis.text.x = element_text(angle=45, hjust = 1))

      # final plot combining both small and large fluxes
      #p <- gridExtra::grid.arrange(small_flux, large_flux, nrow = 2)
      p <- cowplot::plot_grid(small_flux, large_flux, nrow = 2)
      ggiraph::girafe(ggobj = p, width_svg = 8)

    })



  })
}

