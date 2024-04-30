# list of floats equipped with UVP6 and OST
wmo_list <- data.table::fread('data-raw/wmo_list.csv')

# save data as .rda file
usethis::use_data(wmo_list, overwrite = TRUE)
