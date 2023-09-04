#' @description Function that extracts UVP6 particle data from a R trajectory file (NetCDF)
#'
#' @param ncfile
#'
#' @return a tibble with 19 columns (18 size classes + metadata (depth, parking depth, cycle number and date))
#'
#' @export
#'
#' @noRd
#'
#' @example extract_LPM("/data1/GDAC/AUX/coriolis/4903634/4903634_Rtraj_aux.nc")
#'

extract_LPM <- function(ncfile){

  # open NetCDF
  nc_data <- try(ncdf4::nc_open(ncfile))
  if(class(nc_data) == 'try-error'){
    return(0)
  }else{
    # extract pressure field
    pres <- ncdf4::ncvar_get(nc_data, 'PRES')
    # extract measurement code
    mc <- ncdf4::ncvar_get(nc_data, 'MEASUREMENT_CODE')
    # extract time
    juld <- ncdf4::ncvar_get(nc_data, 'JULD')
    # extract cycle number
    cycle <- ncdf4::ncvar_get(nc_data, 'CYCLE_NUMBER')
    # extract wmo (and remove leading and trailing whitespaces)
    wmo <- stringr::str_trim(ncdf4::ncvar_get(nc_data, 'PLATFORM_NUMBER'))

    # extract particle size spectra
    part_spectra <- try(ncdf4::ncvar_get(nc_data, 'NB_SIZE_SPECTRA_PARTICLES'))

    if(class(part_spectra)[1] != 'try-error'){

      # transpose part spectra matrix
      part_spectra <- tibble::as_tibble(t(part_spectra))
      # lpm classes
      lpm_classes <- c('NP_Size_50.8','NP_Size_64','NP_Size_80.6', 'NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
                       'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
                       'NP_Size_1630','NP_Size_2050','NP_Size_2580')
      # rename columns
      colnames(part_spectra) <- lpm_classes

      # extract number of images
      image_number <- ncdf4::ncvar_get(nc_data, 'IMAGE_NUMBER_PARTICLES_LPM')

      # divide particle concentrations by number of images
      part_spectra <- part_spectra %>% dplyr::mutate(dplyr::across(NP_Size_50.8:NP_Size_2580, ~.x/(0.7*image_number))) # 0.7 = UVP6 image volume

      # add depth/juld/cycle to part_spectra
      part_spectra$depth <- pres
      part_spectra$mc <- mc
      part_spectra$cycle <- cycle
      part_spectra$juld <- juld

      # convert julian day to human time
      part_spectra <- part_spectra %>% dplyr::mutate(juld = oce::argoJuldToTime(juld))

      # drop NA in particle size spectra
      part_spectra <- part_spectra %>% tidyr::drop_na(NP_Size_50.8)

      # keep data when the float is parked
      part_spectra <- part_spectra %>%
        dplyr::filter(mc == 290) %>%
        dplyr::select(-mc)

      # define "standard" parking depths (200 m, 500 m and 1000 m)
      part_spectra <- part_spectra %>% dplyr::mutate(park_depth = dplyr::if_else(depth < 350, '200 m', dplyr::if_else(depth > 750, '1000 m', '500 m')))

      # reorder tibble
      part_spectra <- part_spectra %>% dplyr::select(depth, park_depth, cycle, juld, dplyr::everything())

      # remove some weird numbers associated to BAD QCs (but all QCs are at 0 so not JULD_QC is not useful here)
      part_spectra <- part_spectra %>% dplyr::filter(juld > '2020-01-01', juld < '2025-01-01')

      # close NetCDF
      ncdf4::nc_close(nc_data)

      return(part_spectra)
    }else{ # no particle data in the NetCDF
      return(0)
    }
  }
}
