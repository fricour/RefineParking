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
    # needed to show progress bar
    # waiter::use_hostess(), -> BUG, see issues on github with the waiter package
    waiter::useAttendant(),
    # url <- "https://www.freecodecamp.org/news/content/images/size/w2000/2020/04/w-qjCHPZbeXCQ-unsplash.jpg",
    # waiter::waiterPreloader(html = h1("Science is on its way !"), image = url),
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
            shiny::markdown("
                            More info on the UVP6: [paper](https://aslopubs.onlinelibrary.wiley.com/doi/10.1002/lom3.10475) and [manufacturer](http://www.hydroptic.com/index.php/public/Page/product_item/UVP6-LP) <br>
                            <br>
                            More info on the embedded classification algorithm (UVPec): [Zenodo](https://zenodo.org/records/10694204) - [Pypi](https://pypi.org/project/uvpec/) - [Git](https://github.com/ecotaxa/uvpec)
                            ")
          )#,
          #footer = tags$p("Footer test") # need to wait for the fix to be applied on bslib https://github.com/rstudio/bslib/issues/1024
        ),
        navset_card_tab(
          id = "ost-tab",
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
