# setup ---------
source("R/setup.R")

## read functions --------------
source("R/functions.R")


## get lined up medians --------
source("R/get_linedup_median.R")  # this takes a while.


## by vars ---------
by_vars <- c("country_code", "reporting_year", "reporting_level", "welfare_type")


# format data -----------

## filter and SPL ------
med2 <- med |>
  fsubset(!(country %in% c("CHN", "IDN", "IND") &
              reporting_level != "national")) |>
  ftransform(spl = wbpip:::compute_spl(median, ppp_year))


# calculate SPR -------------

spl <- med2 |>
  fselect(country,
          povline = spl,
          year,
          welfare_type)

poss_pip <- purrr::possibly(pipapi::pip)
spr <-
  purrr::pmap(spl,
              \(country, year, povline, welfare_type) {
    y <-
      poss_pip(country      = country,
              year         = year,
              povline      = povline,
              lkup         = lkup,
              fill_gaps    = TRUE,
              welfare_type = welfare_type)

    if (is.null(y)) {
     y <- data.table(country_code   = country,
                     reporting_year = year,
                     welfare_type  = welfare_type,
                     poverty_line  = povline)
    }

    y
  },
  .progress = TRUE
  ) |>
  rowbind(fill = TRUE) |>
  fgroup_by(country_code) |>
  fmutate(country_name  =
            na_focb(na_locf(country_name)),
          reporting_level =
            na_focb(na_locf(reporting_level))) |>
  fungroup()


spr  %<>%
  fselect(country_code,
          reporting_year,
          reporting_level,
          welfare_type,
          spl = poverty_line,
          spr = headcount)

d_med <- med |>
  frename(country = country_code,
          year = reporting_year)


# join the median by reporting level
d_spr <- joyn::joyn(spr, d_med,
                    by = by_vars,
                    match_type = "1:1",
                    reportvar = FALSE)



# save ---------

## Project dir --------------
spl_datadir <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE)


fst::write_fst(d_spr, fs::path(spl_datadir, "spr_lnp", ext = "fst"))
haven::write_dta(d_spr, fs::path(spl_datadir, "spr_lnp", ext = "dta"))

spr_id <- joyn::is_id(d_spr,by_vars, return_report = TRUE )
spr_id[copies > 1]


# spr <- fst::read_fst(fs::path(spl_datadir, "spr_lnp", ext = "fst"))
fst::write_fst(d_spr, fs::path(gls$OUT_AUX_DIR_PC, "spr_lnp", ext = "fst"))





# data.table way.
# rdt0 <- dt  |>
#   ftransform(diff_med = abs(.5 - headcount)) |>
#   {\(.) .[.[, .I[diff_med == min(diff_med, na.rm = TRUE)],
#             by = by_vars]$V1]}() |>
#   na_omit()

