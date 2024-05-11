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
    actionButton(inputId = ns("run_computation"), label = "Compute OST fluxes (only needed when adding or removing floats)"),
    # https://github.com/JohnCoene/waiter/issues/132 -> needed to solve the waiter not showing on first run
    # div(
    #   id = ns("ggiraph-container"),
    #   ggiraph::girafeOutput(ns("plot_parking_OST"))
    # )
    waiter::attendantBar(ns("progress-bar")),
    ggiraph::girafeOutput(ns("plot_parking_OST"))
  )
}

#' ost Server Functions
#'
#' @noRd
mod_ost_server <- function(id, user_float, float_colour_zone, path_to_floats_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create waiter
    # host <- waiter::Hostess$new(infinite = TRUE)
    # w <- waiter::Waiter$new(ns("plot_parking_OST"),
    #                         color = "white",
    #                         html = host$get_loader(
    #                           preset = "circle",
    #                           text_color = "black",
    #                           class = "label-center",
    #                           center_page = FALSE
    #                         ))
    # w <- Waiter$new(
    #   color = "white",
    #   html = waiter::attendantBar(
    #     ns("progress-bar"),
    #     width = 200, # MUST be set with waiter
    #     text = "loading stuff"
    #   )
    # )

    att <- waiter::Attendant$new(ns("progress-bar"), hide_on_max = TRUE)

    # color mapping
    colours <- c(
      'Labrador Sea' = '#E41A1C',
      'Arabian Sea' = '#377EB8',
      'Guinea Dome' = '#4DAF4A',
      'Apero mission' = '#984EA3',
      'West Kerguelen' = '#FF7F00',
      'East Kerguelen' = '#FFFF33',
      'Tropical Indian Ocean' = '#A65628',
      'South Pacific Gyre' = '#F781BF',
      'Nordic Seas' = '#125112',
      'North Pacific Gyre' = '#91C5F0',
      'California Current' = '#999999')

    # OST data to plot
    ost_data <- eventReactive(input$run_computation, {
      req(c(user_float$park_depth()))

      # start waiter
      # w$show()
      # host$start()
      #w$show()
      att$auto()

      # derive ost data
      if(is.null(user_float$region())){
        tmp <- purrr::map_dfr(user_float$wmo(), extract_ost_data, path_to_data = path_to_floats_data, .progress = FALSE)
      }else{
        selected_float <- float_colour_zone %>%
          dplyr::filter(zone %in% user_float$region()) %>%
          dplyr::pull(wmo)
        tmp <- purrr::map_dfr(selected_float, extract_ost_data, path_to_data = path_to_floats_data, .progress = FALSE)
      }
      # for plotting
      tmp <- tmp %>%
        dplyr::mutate(park_depth = factor(park_depth, levels = c('200 m', '500 m', '1000 m'))) %>%
        dplyr::mutate(min_time = as.Date(min_time))
      # add colour scheme
      tmp <- merge(tmp, float_colour_zone)

      # close waiter
      # host$close()
      on.exit({
        att$done()
        #w$hide()
      })
      #w$hide()
      return(tmp)
    })


    # render plot
    output$plot_parking_OST <- ggiraph::renderGirafe({
      shiny::validate(shiny::need(nrow(ost_data()) > 0, message = "Enter a valid WMO or a float that is equipped with the OST."))

      # filter data on parking depth
      plot_data <- ost_data() %>%
        dplyr::filter(park_depth == user_float$park_depth()) %>%
        dplyr::mutate(total_flux = small_flux + large_flux) %>%
        dplyr::mutate(colour_depth = dplyr::case_when(
          park_depth == '200 m' ~ '#fde725',
          park_depth == '500 m' ~ '#21908c',
          park_depth == '1000 m' ~ '#440154')
        )

      if(user_float$region_colour()){

          small_flux <- plot_data %>% ggplot() +
            geom_point_interactive(aes(x = min_time, y = small_flux, colour = zone, shape = park_depth, tooltip = round(small_flux, 2))) +
            scale_colour_manual(values = colours) +
            theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{small}$')) +
            scale_y_continuous(trans = 'log10') +
            theme(legend.position = "top") +
            theme(text = element_text(size = 10)) +
            labs(shape = '', colour = '') +
            scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
            theme(axis.text.x = element_text(angle=45, hjust = 1)) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())

        large_flux <- plot_data %>% ggplot() +
          geom_point_interactive(aes(x = min_time, y = large_flux, colour = zone, shape = park_depth, tooltip = round(large_flux, 2))) +
          scale_colour_manual(values = colours) +
          theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{large}$')) +
          scale_y_continuous(trans = 'log10') +
          theme(legend.position = "none") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1)) +
          theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())

        total_flux <- plot_data %>% ggplot() +
          geom_point_interactive(aes(x = min_time, y = total_flux, colour = zone, shape = park_depth, tooltip = round(total_flux, 2))) +
          scale_colour_manual(values = colours) +
          theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{total}$')) +
          scale_y_continuous(trans = 'log10') +
          theme(legend.position = "none") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1))

      # final plot combining both small and large fluxes
      p <- patchwork::wrap_plots(small_flux, large_flux, total_flux, nrow = 3)
      ggiraph::girafe(ggobj = p, width_svg = 8)
      }else{
        small_flux <- plot_data %>% ggplot() +
          geom_point_interactive(aes(x = min_time, y = small_flux, colour = factor(wmo), shape = park_depth, tooltip = round(small_flux, 2))) +
          theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{small}$')) +
          scale_y_continuous(trans = 'log10') +
          theme(legend.position = "top") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1)) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

        large_flux <- plot_data %>% ggplot() +
          geom_point_interactive(aes(x = min_time, y = large_flux, colour = factor(wmo), shape = park_depth, tooltip = round(large_flux, 2))) +
          theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{large}$')) +
          scale_y_continuous(trans = 'log10') +
          theme(legend.position = "none") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1)) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())

        total_flux <- plot_data %>% ggplot() +
          geom_point_interactive(aes(x = min_time, y = total_flux, colour = factor(wmo), shape = park_depth, tooltip = round(total_flux, 2))) +
          theme_bw() + labs(x = 'Date', y = latex2exp::TeX('$F_{total}$')) +
          scale_y_continuous(trans = 'log10') +
          theme(legend.position = "none") +
          theme(text = element_text(size = 10)) +
          labs(shape = '', colour = '') +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1))

        p <- patchwork::wrap_plots(small_flux, large_flux, total_flux, nrow = 3)
        ggiraph::girafe(ggobj = p, width_svg = 8)

      }
    })



  })
}

