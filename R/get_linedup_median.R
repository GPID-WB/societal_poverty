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
          povline = predicted_mean_ppp ) |>
  na_omit() |>  # check this, as there should not be NAs
  funique()

i = 1
implicit_povline(goal           = 0.5,
                povline         = cy$povline[i],
                country         = cy$country[i],
                year            = cy$year[i],
                fill_gaps       = TRUE,
                reporting_level = cy$reporting_level[i],
                lkup            = lkup)



cy <- cy[1:10]

lmed <- purrr::pmap_dbl(cy,
                        implicit_povline,
                        lkup = lkup,
                        .progress = TRUE)


med <- add_vars(cy, median = lmed) |>
  fselect(-povline)

