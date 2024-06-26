#' uvp6 UI Function
#'
#' @description On the basis of WMOs (either individual WMOs or regions) and selected size classes (from 102 microns to 2.5 mm), this module recovers particle concentration.
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

  # creation of input controls specific to the particle concentration plot
  uvp6_gear <- popover(
    bsicons::bs_icon("gear", size = "1em"),
    numericInput(inputId = ns("uvp_point_size"),
                 label = "Point size",
                 value = 2,
                 min = 0.1,
                 step = 1,
                 max = Inf
                ),
    numericInput(inputId = ns("uvp_max_abundance"),
                 label = "Max abundance",
                 value = 10,
                 min = 0,
                 max = Inf
                 ),
    checkboxInput(inputId = ns("show_all_classes"),
                  label = "Show all size classes",
                  value = F
                 ),
    checkboxInput(inputId = ns("free_y_scale"),
                  label = "Free y scales",
                  value = TRUE
                  ),
    title = "Input controls"
  )

  tagList(
        uvp6_gear,
        girafeOutput(ns("plot_parking_uvp"), height = "700px", width = "100%")
  )

}

#' uvp6 Server Functions
#'
#' @noRd
mod_uvp6_server <- function(id, user_float, float_colour_zone, path_to_floats_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

    # add debounce option if floats are selected too fast
    float_d <- user_float$wmo %>% debounce(1000)

    # add debounce option if size classes are selected too fast
    size_class_d <- user_float$size_class %>% debounce(100)

    # UVP6 data to plot
    particle_data <- reactive({
      shiny::validate(shiny::need(((!is.null(size_class_d())) || (input$show_all_classes)) && ((!is.null(user_float$region())) || (!is.null(float_d))), message = "Select a float (or a region) and a size class (or all size classes)."))

      # compute daily mean particle concentration for all floats given in input
      if(is.null(user_float$region())){
        if(input$show_all_classes){
          size_class_to_show <- lpm_classes
        }else{
          size_class_to_show <- size_class_d()
        }
        # compute particle concentration
        tmp <- purrr::map_dfr(float_d(), compute_daily_mean_part_conc, lpm_classes = size_class_to_show, path_to_data = path_to_floats_data, .progress = FALSE)
      }else{
        selected_float <- float_colour_zone %>%
          dplyr::filter(zone %in% user_float$region()) %>%
          dplyr::pull(wmo)
        if(user_float$show_all_classes()){
          size_class_to_show <- lpm_classes
        }else{
          size_class_to_show <- size_class_d()
        }
        # compute particle concentration
        tmp <- purrr::map_dfr(selected_float, compute_daily_mean_part_conc, lpm_classes = size_class_to_show, path_to_data = path_to_floats_data, .progress = FALSE)
      }
      # add colour based on the region
      tmp <- merge(tmp, float_colour_zone)
      # add colour for parking depth levels (not used at the moment but I keep it for later use)
      tmp <- tmp %>% dplyr::mutate(colour_depth = dplyr::case_when(
        park_depth == '200 m' ~ '#fde725',
        park_depth == '500 m' ~ '#21908c',
        park_depth == '1000 m' ~ '#440154')
      ) %>%
      dplyr::mutate(park_depth = factor(park_depth, levels = c('200 m', '500 m', '1000 m')))

      return(tmp)
    })

    # render plot of particle concentration
    output$plot_parking_uvp <- renderGirafe({
      shiny::validate(shiny::need(nrow(particle_data()) > 0, message = "Error retrieving UVP6 data."))
      shiny::validate(shiny::need(input$uvp_max_abundance > 0, message = "Max abundance should be > 0."))
      shiny::validate(shiny::need(!is.null(user_float$park_depth()), message = "Select a depth."))

      if(input$free_y_scale){
        if(user_float$region_colour()){
        p <- particle_data() %>% dplyr::filter(park_depth %in% user_float$park_depth()) %>% ggplot() +
          geom_point_interactive(aes(juld, mean_conc, shape = park_depth, colour = zone, tooltip = round(mean_conc, 2)), size = input$uvp_point_size) +
          scale_colour_manual(values = c('Labrador Sea' = '#E41A1C',
                                         'Arabian Sea' = '#377EB8',
                                         'Guinea Dome' = '#4DAF4A',
                                         'Apero mission' = '#984EA3',
                                         'West Kerguelen' = '#FF7F00',
                                         'East Kerguelen' = '#FFFF33',
                                         'Tropical Indian Ocean' = '#A65628',
                                         'South Pacific Gyre' = '#F781BF',
                                         'Nordic Seas' = '#125112',
                                         'North Pacific Gyre' = '#91C5F0',
                                         'California Current' = '#999999')) +
          theme_bw() + labs(x = 'Date', y = 'Particle abundance (#/L)') +
          scale_y_continuous(trans = 'log10') +
          facet_wrap(~size, scales = 'free_y', labeller = as_labeller(facet_all)) +
          theme(legend.position = "top") +
          labs(shape = '', colour = '') +
          theme(text = element_text(size = 10)) +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1))
        }else{
        p <- particle_data() %>% dplyr::filter(park_depth %in% user_float$park_depth()) %>% ggplot() +
          geom_point_interactive(aes(juld, mean_conc, shape = park_depth, colour = wmo, tooltip = round(mean_conc, 2)), size = input$uvp_point_size) +
          theme_bw() + labs(x = 'Date', y = 'Particle abundance (#/L)') +
          scale_y_continuous(trans = 'log10') +
          facet_wrap(~size, scales = 'free_y', labeller = as_labeller(facet_all)) +
          theme(legend.position = "top") +
          labs(shape = '', colour = '') +
          theme(text = element_text(size = 10)) +
          scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
          theme(axis.text.x = element_text(angle=45, hjust = 1))
        }
        # ggiraph the plot to make it nicer
        p <- ggiraph::girafe(ggobj = p, width_svg = 8)
      }else{
        if(user_float$region_colour()){
          p <- particle_data() %>%
            dplyr::filter(park_depth %in% user_float$park_depth()) %>%
            dplyr::filter(mean_conc <= input$uvp_max_abundance) %>% ggplot() +
            geom_point_interactive(aes(juld, mean_conc, shape = park_depth, colour = zone, tooltip = round(mean_conc, 2)), size = input$uvp_point_size) +
            scale_colour_manual(values = c('Labrador Sea' = '#E41A1C',
                                           'Arabian Sea' = '#377EB8',
                                           'Guinea Dome' = '#4DAF4A',
                                           'Apero mission' = '#984EA3',
                                           'West Kerguelen' = '#FF7F00',
                                           'East Kerguelen' = '#FFFF33',
                                           'Tropical Indian Ocean' = '#A65628',
                                           'South Pacific Gyre' = '#F781BF',
                                           'Nordic Seas' = '#125112',
                                           'North Pacific Gyre' = '#91C5F0',
                                           'California Current' = '#999999')) +
            theme_bw() + labs(x = 'Date', y = 'Particle abundance (#/L)') +
            scale_y_continuous(trans = 'log10') +
            facet_wrap(~size, scales = 'free_y', labeller = as_labeller(facet_all)) +
            theme(legend.position = "top") +
            labs(shape = '', colour = '') +
            theme(text = element_text(size = 10)) +
            scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
            theme(axis.text.x = element_text(angle=45, hjust = 1))

        # ggiraph the plot to make it nicer
        p <- ggiraph::girafe(ggobj = p, width_svg = 8)
        }else{
          p <- particle_data() %>%
            dplyr::filter(park_depth %in% user_float$park_depth()) %>%
            dplyr::filter(mean_conc <= input$uvp_max_abundance) %>% ggplot() +
            geom_point_interactive(aes(juld, mean_conc, shape = park_depth, colour = wmo, tooltip = round(mean_conc, 2)), size = input$uvp_point_size) +
            theme_bw() + labs(x = 'Date', y = 'Particle abundance (#/L)') +
            scale_y_continuous(trans = 'log10') +
            facet_wrap(~size, scales = 'free_y', labeller = as_labeller(facet_all)) +
            theme(legend.position = "top") +
            labs(shape = '', colour = '') +
            theme(text = element_text(size = 10)) +
            scale_x_date(labels = scales::date_format("%b/%y"), date_breaks = '3 month') +
            theme(axis.text.x = element_text(angle=45, hjust = 1))

          # ggiraph the plot to make it nicer
          p <- ggiraph::girafe(ggobj = p, width_svg = 8)
        }
      }
    })
  })
}
