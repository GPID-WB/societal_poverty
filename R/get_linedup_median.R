# setup ---------
source("R/setup.R")

## read functions --------------
source("R/functions.R")


## get countries and year -------------

cy <- gls$OUT_AUX_DIR_PC |>
  fs::path("interpolated_means.qs") |>
  qs::qread() |>
  fselect(country = country_code,
          year    = reporting_year,
          reporting_level,
          povline = predicted_mean_ppp,
          welfare_type) |>
  na_omit() |>  # check this, as there should not be NAs
  funique()


# cy <- cy[1:10]

# lmed <- purrr::pmap_dbl(cy,
#                         implicit_povline,
#                         lkup = lkup,
#                         .progress = TRUE)

lmed <- purrr::pmap(cy[1],
                    implicit_povline,
                    lkup = lkup,
                    complete_return = TRUE,
                    .progress = list(format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | rate [{cli::pb_rate}] | ETA: {cli::pb_eta}"))

# lmed <- purrr::pmap_dbl(cy,
#                         implicit_povline,
#                         lkup = lkup,
#                         .progress = list(format = " {cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | rate [{cli::pb_rate}] | ETA: {cli::pb_eta}"))
#

med <- add_vars(cy, median = lmed) |>
  fselect(country,
          year,
          reporting_level,
          welfare_type,
          median)

