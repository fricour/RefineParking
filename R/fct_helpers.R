#' @description Function that extracts the daily average particle concentration (from the UVP6) by a given float
#'
#' @param wmo WMO of a float (numeric)
#'
#' @return a tibble
#'
#' @export
#'
#' @noRd
#'
#' @example compute_daily_mean_part_conc(6904240)
compute_daily_mean_part_conc <- function(wmo, lpm_classes, path_to_data){

  # particle size classe
  # lpm_classes <- c('NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
  #                  'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
  #                  'NP_Size_1630','NP_Size_2050')

  ncfile <- paste0(path_to_data,wmo,'/',wmo,'_Rtraj_aux.nc')

  if (file.exists(ncfile)) {
    uvp6_data <- extract_LPM(ncfile)
    # reformat data
    uvp6_data <- uvp6_data %>%
      tidyr::pivot_longer(cols = lpm_classes, names_to = 'size', values_to = 'conc') %>%
      dplyr::mutate(size = size %>% stringr::str_remove('NP_Size_') %>% as.numeric()) %>%
      dplyr::mutate(juld = lubridate::as_date(juld)) #%>%
      #dplyr::filter(size >= 102, size < 2050)

    # compute daily mean particle concentration at each cycle and at each drifting depth
    mean_uvp6_data <- uvp6_data %>% dplyr::group_by(cycle, size, park_depth, juld) %>% dplyr::summarize(mean_conc = mean(conc, na.rm=T))
    # add wmo
    mean_uvp6_data$wmo <- wmo
    return(mean_uvp6_data)
  }else{ # no UVP6 particle data
    return(NULL)
  }
}


#' @description Function that extracts a variable (in ADJUSTED mode) if possible + its QC from a Rtrajectory file
#'
#' @param ncdata an open NetCDF file
#' @param parameter a BGC-ARGO parameter
#'
#' @return a tibble
#'
#' @export
#'
#' @noRd
#'
#' @example extract_parameter(nc_data, 'BBP700')
#'
#'
extract_cp_data <- function(wmo, path_to_data){

  ncfile <- paste0(path_to_data,wmo,'/',wmo,'_Rtraj.nc')

  # open netcdf
  nc_data <- ncdf4::nc_open(ncfile) # Rtraj files should ALWAYS exist so I am not testing it

  # extract parameter
  #value <- try(ncdf4::ncvar_get(nc_data, 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660'))
  value <- try(ncdf4::ncvar_get(nc_data, 'CP660'))
  depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
  mc <- ncdf4::ncvar_get(nc_data, 'MEASUREMENT_CODE')
  juld <- ncdf4::ncvar_get(nc_data, 'JULD')
  juld <- oce::argoJuldToTime(juld)
  cycle <- ncdf4::ncvar_get(nc_data, 'CYCLE_NUMBER')
  # extract wmo (and remove leading and trailing whitespaces)
  wmo <- stringr::str_trim(ncdf4::ncvar_get(nc_data, 'PLATFORM_NUMBER'))

  # check state of parameter
  if(class(value)[1] == 'try-error'){ # parameter does not exist, return empty tibble
    return(tibble::tibble(juld = NA, depth = NA, mc = NA, value = NA, qc = NA))
  }else{
    # check for adjusted values for that parameter
    value_adjusted <- try(ncdf4::ncvar_get(nc_data, 'CP660_ADJUSTED'))
    if(class(value_adjusted)[1] == 'try-error'){ # ADJUSTED field does not exist in NetCDF file for now
      qc <- ncdf4::ncvar_get(nc_data, 'CP660_QC')
      qc <- as.numeric(unlist(strsplit(qc,split="")))
    }else if(all(is.na(value_adjusted)) == TRUE){ # if TRUE, there are no adjusted values
      qc <- ncdf4::ncvar_get(nc_data, 'CP660_QC')
      qc <- as.numeric(unlist(strsplit(qc,split="")))
    }else{ # there are adjusted values
      qc <- ncdf4::ncvar_get(nc_data, 'CP660_ADJUSTED_QC')
      qc <- as.numeric(unlist(strsplit(qc,split="")))
    }
  }

  # make final tibble
  tib <- tibble::tibble(wmo = wmo, cycle = cycle, juld = juld, mc = mc, depth = depth, cp = value, qc = qc)
  # clean data
  tib <- tib %>%
    dplyr::filter(cycle >= 1, mc == 290) %>%
    tidyr::drop_na(cp) %>%
    dplyr::mutate(park_depth = dplyr::if_else(depth < 350, '200 m', dplyr::if_else(depth > 750, '1000 m', '500 m'))) %>%
    dplyr::select(-mc)

  # # convert cp data to physical data
  # CSCdark <- RefineParking::c_rover_calib[RefineParking::c_rover_calib$WMO == wmo,]$CSCdark
  # CSCcal <- RefineParking::c_rover_calib[RefineParking::c_rover_calib$WMO == wmo,]$CSCcal
  # x <- 0.25
  # tib <- tib %>% dplyr::mutate(cp = -log((cp - CSCdark)/(CSCcal-CSCdark))/x)

  # close nc file
  ncdf4::nc_close(nc_data)

  return(tib)
}

#'
#'
#'
#'
#'
extract_ost_data <- function(wmo_float, path_to_data){

  # parking depths (so far we only have those 3 but that might change in the future)
  park_depth <- c('200 m', '500 m', '1000 m')

  # extract cp data from the float
  data <- extract_cp_data(wmo_float, path_to_data)

  res <- data.frame()
  max_cycle <- max(data$cycle)
  for (i in park_depth){
    for (j in seq(1:max_cycle)){
      tmp <- data %>% dplyr::filter(park_depth == i, cycle == j)
      if(nrow(tmp) == 0){ # no data for this cycle or at this parking depth
        next
      }else if(nrow(tmp) < 3){ # case where there is not enough data
        next
      }else{
        output <- derive_ost_flux(tmp, wmo_float)
        res <- rbind(res, output)
      }
    }
  }

  return(res)

}

## FROM JEAN-OLIVIER IRISSON, see https://github.com/jiho/castr/blob/master/R/slide.R
#' Apply a function in a sliding window along a vector
#'
#' Allows to compute a moving average, moving median, or even moving standard deviation, etc. in a generic way.
#'
#' @param x input numeric vector.
#' @param k order of the window; the window size is 2k+1.
#' @param fun function to apply in the moving window.
#' @param n number of times to pass the function over the data.
#' @param ... arguments passed to `fun`. A usual one is `na.rm=TRUE` to avoid getting `NA`s at the extremities of `x`.
#'
#' @details A window of size `2k+1` is centred on element `i` of `x`. All elements from index `i-k` to index `i+k` are sent to function `fun`. The returned value is associated with index `i` in the result. The window is moved to element `i+1` and so on.
#'
#' For such sliding window computation to make sense, the data must be recorded on a regular coordinate (i.e. at regular intervals). Otherwise, data points that are far from each other may end up in the same window.
#'
#' The extremeties of the input vector are padded with `NA` to be able to center the sliding window from the first to the last elements. This means that, to avoid getting `k` missing values at the beginning and at the end of the result, `na.rm=TRUE` should be passed to `fun`.
#'
#' @return The data passed through `fun`, `n` times.
#' @export
#'
#' @seealso [cweights()] to compute weights centered in the middle of the window.
#'
#' @examples
#' # create some data and add random noise
#' xs <- sin(seq(0, 4*pi, length=100))
#' x <- xs + rnorm(length(xs), sd=0.25)
#' plot(x)
#' lines(xs)
#' # filter the data in various ways
#' # moving average
#' mav   <- slide(x, 3, mean, na.rm=TRUE)
#' # running moving average
#' rmav  <- slide(x, 3, mean, na.rm=TRUE, n=4)
#' # weighted moving average
#' wmav  <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3))
#' # weighted running moving average
#' wrmav <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3), n=4)
#' # moving median
#' mmed  <- slide(x, 3, median, na.rm=TRUE)
#' lines(mav, col="red")
#' lines(rmav, col="red", lty="dashed")
#' lines(wmav, col="orange")
#' lines(wrmav, col="orange", lty="dashed")
#' lines(mmed, col="blue")
#' # inspect variability around filtered data
#' plot(slide(x-rmav, 7, sd))
#' plot(slide(x-mmed, 7, mad))
slide <- function(x, k, fun, n=1, ...) {
  # make sure to get a function as the `fun` argument (i.e. avoid name masking)
  if (!is.function(fun)) {
    fun <- get(as.character(substitute(fun)), mode="function")
  }

  if (n>=1) {
    # repeat n times
    for (t in 1:n) {
      # pad the extremities of data to be able to compute over the whole vector
      x <- c(rep(NA, times=k), x, rep(NA, times=k))

      # apply the rolling function (and remove padding at the extremities)
      x <- sapply((k+1):(length(x)-k), function(i) {
        fun(x[(i-k):(i+k)], ...)
      })
    }
  }

  return(x)
}
