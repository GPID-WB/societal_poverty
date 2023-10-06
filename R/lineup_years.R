# Load libraries -----------
library(fastverse)
# pak::pak("PIP-technical-team/pipapi@DEV")
pak::pak("PIP-technical-team/pipapi@DEV")

# setup ------------


## directories -----------
force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}

version  <- "20230919_2017_01_02_PROD"

new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version)

## Lkup files ===========
lkups <- pipapi::create_versioned_lkups(data_dir = data_dir,
                                        vintage_pattern = version)


# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <-  lkups$versions_paths[[lkups$latest_release]]

## loau data --------
dt <- fst::read_fst(fs::path(new_dir,
                             "raw_recovered_dist", ext = "fst"),
                    as.data.table = TRUE)
#
# write_parquet(dt, fs::path(new_dir,
#                           "raw_recovered_dist"))
#


## by vars ---------
by_vars <- c("country_code", "reporting_year", "reporting_level", "welfare_type")


# format data -----------

# I tried using arrow but it does not support window calculations and joining the databases is super slow. I also tried data.table way but it is slower than collapse.

## get the min difference ----------
med <- dt  |>
  ftransform(diff_med = abs(.5 - headcount)) |>
  fgroup_by(by_vars) |>
  fmutate(gmin = (fmin(diff_med) == diff_med)) |>
  fungroup() |>
  fsubset(gmin)

## Find duplicates ---------
med_dup <-  duplicated(med, by = c(by_vars, "diff_med"))

## filter and SPL ------
med <- med |>
  fsubset(!med_dup) |>
  fselect(c(by_vars, median = "poverty_line")) |>
  ftransform(spl = wbpip:::compute_spl(median, ppp_year))



# calculate SPR -------------


spl <- med |>
  fselect(country = country_code ,
          povline = spl,
          year    = reporting_year,
          welfare_type,
          reporting_level ) |>
  as.list()


spr <-
  purrr::pmap(spl, \(country, year, povline, welfare_type, reporting_level) {
    y <-
      pipapi::pip(country      = country,
                  year         = year,
                  povline      = povline,
                  lkup         = lkup,
                  fill_gaps    = TRUE,
                  welfare_type = welfare_type,
                  reporting_level = reporting_level)
  }
  ) |>
  rbindlist() |>
  fselect(country_code,
          reporting_year,
          reporting_level,
          spl = poverty_line,
          welfare_type,
          spr = headcount)

# save ---------
spl_datadir <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE)


fst::write_fst(spr, fs::path(spl_datadir, "spr_lnp", ext = "fst"))

spr_id <- joyn::is_id(spr,by_vars, return_report = TRUE )
spr_id[copies > 1]

# data.table way.
# rdt0 <- dt  |>
#   ftransform(diff_med = abs(.5 - headcount)) |>
#   {\(.) .[.[, .I[diff_med == min(diff_med, na.rm = TRUE)],
#             by = by_vars]$V1]}() |>
#   na_omit()

