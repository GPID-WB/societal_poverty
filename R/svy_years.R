# setup ---------
source("R/setup.R")

## read functions --------------
source("R/functions.R")

## load data --------

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

mss_med <- joyn::joyn(svy,
                       bsc_med,
                       by             = by_vars,
                       match_type     = "1:1",
                      y_vars_to_keep  = FALSE) |>
  fsubset(.joyn == "x") |>
  fselect(-.joyn) |>
  # remove urban/rural
  fsubset(!(country_code %in% c("CHN") &
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
          welfare_type)

# |>
#   as.list()

### load population data ------------
dt_pop <- pipload::pip_load_aux("pop")


poss_median_from_synth_vec <-
  purrr::possibly(get_median_from_synth_vec,
                  otherwise = data.table(median = NA))
if (nrow(ld_ga) > 0) {
  sv_med <- ld_ga |>
    # loop over all data
    purrr::pmap(poss_median_from_synth_vec) |>
    # create data frame of medians
    rowbind()
  # add original data with metadata of groups data with missing median.
  ga_med <- add_vars(dt_ga, sv_med)
} else {
  ga_med <- data.table(
      country_code      = NA_character_,
      reporting_year    = NA_real_,
      reporting_level   = NA_character_,
      welfare_type      = NA_character_,
      distribution_type = NA_character_,
      median            = NA_real_
  )
}


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
    get_md_median(country, year, welfare_type, version = gls$vintage_dir)
  },
  .progress = TRUE) |>
  unlist() |>
  # create data frame of medians
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
  lapply(c("ga_med", "md_med", "bsc_med"), get) |>
  rowbind(fill = TRUE) |>
  get_vars(vars_to_keep) |>
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
                        y <- pspip(country      = country,
                              year         = year,
                              povline      = povline,
                              welfare_type = welfare_type,
                              lkup         = lkup)
                        if (is.null(y)) {
                          y <- data.table(
                            country_code = country,
                            reporting_year = year,
                            poverty_line = povline,
                            welfare_type = welfare_type
                          )
                        }
                        y
                      },
                      .progress = TRUE)


# find if there are nulls in the data
# ld_spr_null <-  purrr::keep(ld_spr, is.null)
# ld_spr_null[]

# convert to frame and add median

d_med <- d_spl |>
  fselect(country_code,
          reporting_year,
          welfare_type,
          median)

d_spr <- ld_spr |>
  purrr::compact() |>
  rowbind(fill = TRUE) |>
  fselect(country_code,
          reporting_year,
          welfare_type,
          reporting_level,
          spl = poverty_line,
          spr = headcount) |>
  joyn::joyn(d_med,
              match_type = "m:1",
             reportvar = FALSE
             )

# Save ---------

## project dir ----------
spl_datadir <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE)


fst::write_fst(d_spr, fs::path(spl_datadir, "spr_svy", ext = "fst"))
haven::write_dta(d_spr, fs::path(spl_datadir, "spr_svy", ext = "dta"))


# d_spr <- fst::read_fst(fs::path(spl_datadir, "spr_svy", ext = "fst"))
fst::write_fst(d_spr, fs::path(gls$OUT_AUX_DIR_PC, "spr_svy", ext = "fst"))





