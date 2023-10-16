# Load libraries -----------
library(fastverse)
# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/pipapi@DEV")

# setup ------------

## read functionis --------------
source("R/functions.R")

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

### filter group data ------------
dt_ga <- mss_med |>
  fsubset(distribution_type %in% c("aggregate", "group"))

# convert data frame to list to parse it to pmap functions below
ld_ga <-  dt_ga |>
  # fsubset(1:2) |>
  fselect(country  = country_code,
          year     = reporting_year,
          welfare_type) |>
  as.list()

### load population data ------------
dt_pop <- pipload::pip_load_aux("pop")

### synth vector and median =-------
ga_med <- ld_ga |>
  # loop over all data
  purrr::pmap(\(country, year, welfare_type, ...) {
    # load group data and filter mean and population which are already loaded in
    # the global env
    lt <- filter_data(country, year, welfare_type, dt_pop)
    # get synthetic vector for any reporting level available
    sth <- get_synth_vecs(lt)
    # estimate median at the national level
    med <- sth |>
      fsummarise(median = fmedian(welfare, w = weight))
    med
  }) |>
  # create data frame of medians
  rowbind() |>
  # add original data with metadata of groups data with missing median.
  add_vars(dt_ga, pos = "front")



## Microdata ------------
### filter micro data ------------
dt_md <- mss_med |>
  fsubset(distribution_type == "micro")


# convert data frame to list to parse it to pmap functions below
ld_md  <-  dt_md |>
  # fsubset(1:2) |>
  fselect(country  = country_code,
          year     = reporting_year,
          welfare_type) |>
  as.list()

md_med <- ld_md |>
  purrr::pmap(\(country, year, welfare_type, ...){
    get_md_median(country, year, welfare_type)
  }) |>
  # create data frame of medians
  rowbind() |>
  # add original data with metadata of groups data with missing median.
  add_vars(dt_md, pos = "front")


## imputed data --------------
### filter imputed data ------------
dt_id <- mss_med |>
  fsubset(distribution_type == "imputed")




# Annex ----------------
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
