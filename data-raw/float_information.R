# C-Rover calibration
c_rover_calib <- vroom::vroom('data-raw/crover_calibration.csv')

# remove unknown float (see why with Antoine Poteau)
c_rover_calib <- c_rover_calib %>% dplyr::filter(WMO != 7901000)
# remove float not yet deployed
c_rover_calib <- c_rover_calib %>% dplyr::filter(WMO != 4903659)

usethis::use_data(c_rover_calib, overwrite = TRUE)
