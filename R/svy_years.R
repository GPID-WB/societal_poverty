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


## basic medians for most of the countries ------
bsc_med <- pipapi::pip(popshare = .5,
                       lkup = lkup)


bsc_med %<>% # Assignment pipe. It's just cool to use. No other reason
  na_omit(cols = "median")

## load survey means data which has all svy means points -----
svy <- pipapi::get_aux_table(data_dir    = lkup$data_root,
                             table       = "survey_means",
                             long_format = TRUE) |>
  get_vars(c(by_vars, "distribution_type", "survey_mean_ppp"))

mss_med <- joyn::merge(svy,
                       bsc_med,
                       by         = by_vars,
                       match_type = "1:1",
                       yvars      = FALSE) |>
  fsubset(report == "x") |>
  fselect(-report) |>
  # remove urban/rural
  fsubset(!(country_code %in% c("CHN", "IDN", "IND") &
            reporting_level != "national"))

# get medians for special cases ------------


## Group or aggregate data -----------

dt_ga <- mss_med |>
  fsubset(distribution_type %iin% c("aggregate", "group"))

ld_ga <-  dt_ga |>
  fsubset(1:2) |>
  fselect(country  = country_code,
          year     = reporting_year,
          welfare_type) |>
  as.list()
dt_pop <- pipload::pip_load_aux("pop")

ga_med <- ld_ga |>
  purrr::pmap(\(country, year, welfare_type, ...) {
    lt <- filter_data(country, year, welfare_type, dt_pop)

    sth <- get_synth_vecs(lt)
    med <- sth |>
      fsummarise(med = fmedian(welfare, w = weight))
    med
  }) |>
  rowbind() |>
  add_vars(dt_ga[1:2])

### load cache data -----------
i <- 1
country      <- dt_ga$country_code[[i]]
year         <- dt_ga$reporting_year[[i]]
welfare_type <- dt_ga$welfare_type[[i]]
rl           <- "rural"




med <- sth |>
  fsummarise(med = fmedian(welfare, w = weight)) |>
  add_vars(dt_ga[1])

  add_vars(country_code   = country,
           reporting_year = year,
           welfare_type   = welfare_type,
           pos            = "front")
med




mss_med |>
  fselect(distribution_type) |>
  funique()


mss_med |>
  fsubset(distribution_type == "imputed") |>
  fselect(country_code) |>
  funique()




svy[country_code == "CHN" & reporting_year == 1981]


pipapi::pip("CHN", 2018, popshare = .5,
            lkup = lkup)[]

pipapi::pip("CHN", 2018, povline = 5,
            lkup = lkup)[]
