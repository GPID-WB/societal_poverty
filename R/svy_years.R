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
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()


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



## Microdata  and imputed data------------
### filter micro data ------------
dt_md <- mss_med |>
  fsubset(distribution_type %in% c("micro", "imputed"))


# convert data frame to list to parse it to pmap functions below
ld_md  <-  dt_md |>
  # fsubset(1:2) |>
  fselect(country  = country_code,
          year     = reporting_year,
          welfare_type) |>
  as.list()

### estimate median ---------

md_med <- ld_md |>
  purrr::pmap(\(country, year, welfare_type, ...){
    get_md_median(country, year, welfare_type)
  }) |>
  unlist() |>
  # create data frame of medians
  data.table(median = _) |>
  # add original data with metadata of groups data with missing median.
  add_vars(dt_md, pos = "front")



md_med <- md_med |>
  unlist() |>
  data.table(median = _) |>
  # add original data with metadata of groups data with missing median.
  add_vars(dt_md, pos = "front")



# Get SPL ---------------

## append all data bases together ---------------

vars_to_keep <-
  c(
    "country_code",
    "reporting_year",
    "reporting_level",
    "welfare_type",
    "distribution_type",
    "median"
  )

## SPL -------------
d_spl <-
  lapply(c("ga_med", "md_med", "bsc_med"),
         \(x){
           get(x) |>
             get_vars(vars_to_keep)
         }
  ) |>
  rowbind() |>
  fmutate(spl = wbpip:::compute_spl(median, py)) |>
  # remove urban/rural
  fsubset(!(country_code %in% c("CHN", "IDN", "IND") &
              reporting_level != "national"))


setorderv(d_spl, vars_to_keep)


## SPR ---------------------


lp_spr <- d_spl |>
  fselect(country = country_code,
          year    = reporting_year,
          povline = spl,
          welfare_type) |>
  as.list()


pspip <- purrr::possibly(pipapi::pip)


ld_spr <- purrr::pmap(lp_spr,
                      .f = \(country, year, povline, welfare_type){
                        pspip(country      = country,
                              year         = year,
                              povline      = povline,
                              welfare_type = welfare_type,
                              lkup         = lkup)
                      },
                      .progress = TRUE)


# find if there are nulls in the data
ld_spr_null <-  purrr::keep(ld_spr, is.null)

# convert to frame and add median

d_med <- d_spl |>
  fselect(country_code,
          reporting_year,
          welfare_type,
          median)

d_spr <- ld_spr |>
  purrr::compact() |>
  rowbind() |>
  fselect(country_code,
          reporting_year,
          welfare_type,
          reporting_level,
          spl = poverty_line,
          spr = headcount) |>
  joyn::merge(d_med,
              match_type = "m:1",
              reportvar = FALSE)

# Save ---------

## project dir ----------
spl_datadir <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE)


fst::write_fst(d_spr, fs::path(spl_datadir, "spr_svy", ext = "fst"))
haven::write_dta(d_spr, fs::path(spl_datadir, "spr_svy", ext = "dta"))

## TFS dir -----------

gls <- pipfun::pip_create_globals(
  root_dir   = Sys.getenv("PIP_ROOT_DIR"),
  vintage    = version,
  create_dir = FALSE)

# d_spr <- fst::read_fst(fs::path(spl_datadir, "spr_svy", ext = "fst"))
fst::write_fst(d_spr, fs::path(gls$OUT_AUX_DIR_PC, "spr_svy", ext = "fst"))





