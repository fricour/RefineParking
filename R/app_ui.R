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
      title = 'Investigate UVP and OST at parking',
      sidebar = sidebar(
        mod_select_float_ui("sidebar")
      ),
      layout_columns(
        col_widths = c(-3,6,-3,6,6),
        row_heights = c(1,2),
        card(
          card_header("Float map"),
          full_screen = TRUE,
          mod_float_map_ui("float_map")
        ),
        card(
          card_header("UVP6 particle concentration"),
          full_screen = TRUE,
          mod_uvp6_ui("uvp6")
        ),
        card(
          card_header("OST flux"),
          full_screen = TRUE,
          mod_ost_ui("ost")
        )
      )
    )
  )
    # dashboardPage(
    #   dashboardHeader(
    #     title = ''
    #   ),
    #   dashboardSidebar(
    #     mod_select_float_ui("sidebar")
    #   ),
    #   dashboardBody(
    #     mod_float_map_ui("float_map"),
    #     mod_uvp6_ui("uvp6"),
    #     mod_ost_ui("ost"))
    #   )
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
      app_title = "RefineParking"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
