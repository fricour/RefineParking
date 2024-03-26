#' @description Compute the spectral slope from UVP6 particle size spectra (at parking)
#'
#' @param wmo_float WMO of a float
#'
#' @return a tibble
#'
#' @export
#'
#' @noRd
#'
#' @example compute_spectral_slope(6904240)
compute_spectral_slope <- function(wmo_float, path_to_data){

  # extract UVP data at parking
  ncfile <- paste0(path_to_data,wmo_float,'/',wmo_float,'_Rtraj_aux.nc')
  data <- extract_LPM(ncfile)

  # particle size classes
  lpm_classes <- c('NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
                   'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
                   'NP_Size_1630','NP_Size_2050')

  # "center" (pseudo center with a geometric progression of 2/3) of each size bin
  mid_DSE <- c(0.1147968,0.1446349,0.1822286,0.2295937,0.2892699,0.3644572,0.4591873,0.5785398,0.7289145,0.9183747,1.1570796,1.4578289,1.83674934,2.31415916)

  # length of size bin
  size_bin <- c(0.02640633,0.03326989,0.04191744,0.05281267,0.06653979,0.08383488,0.10562533,0.13307958,0.16766976,0.21125066,0.26615915,0.33533952,0.422501323,0.532318310)

  # keep useful columns
  data <- data %>%
    dplyr::mutate(wmo = wmo_float) %>%
    dplyr::select(wmo, juld, cycle, depth, dplyr::all_of(lpm_classes))

  # compute slope
  particle_spectra <- as.matrix(data %>% dplyr::select(dplyr::all_of(lpm_classes)))
  index <- seq(from = 1, to = nrow(particle_spectra), by = 1)
  slopes <- purrr::map_dbl(index, compute_slope, data_spectra = particle_spectra, mid_DSE = mid_DSE, size_bin = size_bin)
  data$spectral_slope <- slopes

  # clean data and compute daily mean slope
  data <- data %>%
    dplyr::select(-dplyr::all_of(lpm_classes)) %>%
    dplyr::mutate(park_depth = dplyr::if_else(depth < 350, '200 m', dplyr::if_else(depth > 750, '1000 m', '500 m'))) %>%
    dplyr::mutate(date = as.Date(juld)) %>%
    dplyr::group_by(wmo, cycle, park_depth, date) %>%
    dplyr::summarize(mean_slope = mean(spectral_slope, na.rm=T))

  return(data)

}

compute_slope <- function(i, data_spectra, mid_DSE, size_bin){

  spectrum <- data_spectra[i,]

  spectrum_norm <- spectrum/size_bin

  # prepare data for linear regression
  Y <- log(spectrum_norm)
  X <- log(mid_DSE)

  # check for finite value
  h <- is.finite(Y)
  Y <- Y[h]
  X <- X[h]

  data_slope <- tibble::tibble(X=X, Y=Y)

  model <- stats::lm(formula = Y ~ X, data = data_slope)
  slope <- model$coefficients[2]

  return(slope)
}

