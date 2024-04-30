#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard bslib
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::use_waiter(),
    # Your application UI logic
    page_sidebar(
      theme = bs_theme(version = 5), # wiser to hard-code it, see https://rstudio.github.io/bslib/articles/dashboards/index.html
      title = 'Marine particles as seen by the UVP6 and the OST',
      sidebar = sidebar(
        mod_select_float_ui("sidebar")
      ),
      layout_columns(
        col_widths = c(6,-6,6,6),
        row_heights = c(1,2,2),
        card(
          card_header("Float map"),
          full_screen = TRUE,
          mod_float_map_ui("float_map")
        ),
        navset_card_tab(
          title = "Underwater Vision Profiler data",
          full_screen = TRUE,
          nav_panel("Particle concentration", mod_uvp6_ui("uvp6")),
          nav_panel("Spectral slope", mod_spectral_slope_ui("spectral_slope")),
          nav_panel(
            shiny::icon("circle-info"),
            shiny::markdown("TODO")
          )
        ),
        navset_card_tab(
          title = "Transmissometer data",
          full_screen = TRUE,
          nav_panel("OST flux", mod_ost_ui("ost")),
          #card_header("OST flux (units: mgC/m²/day)"),
          nav_panel(
            shiny::icon("circle-info"),
            shiny::markdown("TODO")
          )
        ),
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ArgoParticles"
    )
  )
}
