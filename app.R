# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
RefineParking::run_app(path_to_GDAC_CORE_data, path_to_GDAC_AUX_data) # add parameters here (if any)
