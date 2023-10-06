# Load libraries -----------
library(fastverse)
# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/pipapi@DEV")

# setup ------------


## directories -----------
force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}

version  <- "20230919_2017_01_02_PROD"

## Lkup files ===========
lkups <- pipapi::create_versioned_lkups(data_dir        = data_dir,
                                        vintage_pattern = version)

# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <-  lkups$versions_paths[[lkups$latest_release]]

# load data --------

## by vars ---------
by_vars <- c("country_code", "reporting_year", "reporting_level", "welfare_type")


## basic medians for most of the countries
bsc_med <- pipapi::pip(popshare = .5,
                       lkup = lkup)


bsc_med %<>% # Assignment pipe. It's just cool to use. No other reason
  na_omit(cols = "median")

## load survey means data which has all svy means points -----
svy <- pipapi::get_aux_table(data_dir    = lkup$data_root,
                             table       = "survey_means",
                             long_format = TRUE) |>
  get_vars(c(by_vars, "distribution_type"))

mss_med <- joyn::merge(svy,
                       bsc_med,
                       by         = by_vars,
                       match_type = "1:1",
                       yvars      = FALSE) |>
  fsubset(report == "x") |>
  fselect(-report)





svy[country_code == "CHN" & reporting_year == 1981]


pipapi::pip("CHN", 2018, popshare = .5,
            lkup = lkup)[]

pipapi::pip("CHN", 2018, povline = 5,
            lkup = lkup)[]
