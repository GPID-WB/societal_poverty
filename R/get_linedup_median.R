# setup ---------
# source("R/setup.R")  ## un comment for running standalone

## read functions --------------
source("R/functions.R")

lup_medians_f <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE) |>
  fs::path("lup_medians", ext = "fst")

if (fs::file_exists(lup_medians_f)) {
  med <- fst::read_fst(lup_medians_f, as.data.table = TRUE)
} else {
  med <- NULL
}


## get countries and year -------------

cy <- gls$OUT_AUX_DIR_PC |>
  fs::path("interpolated_means.qs") |>
  qs::qread() |>
  fgroup_by(country_code, reporting_year, reporting_level, welfare_type) |>
  fsummarise(median = fmean(predicted_mean_ppp, w = relative_distance),
             pop     = ffirst(reporting_pop)) |>
  fungroup() |>
  _[, n := .N,
    by = .(country_code, reporting_year, welfare_type)]

cy1 <- cy[n == 1
          ][,
            c("n", "pop") := NULL]

cy2 <- cy |>
  fsubset(n == 2) |>
  fgroup_by(country_code, reporting_year, welfare_type) |>
  fsummarise(median        = fmean(median, w = pop)) |>
  fungroup() |>
  ftransform(reporting_level = "national")


cy <- rowbind(cy1, cy2) |>
  frename(country_code = country,
          reporting_year = year) |>
  na_omit() |>  # check this, as there should not be NAs
  funique()


# for testing only
# srows <- sample(1:nrow(cy), 10, replace = FALSE)
# med <- cy[srows
#           ][, median := srows]

# if I want to get estimates only in a subset
if (is.character(ct)) {
  cy <- cy[country %in% ct ]
}
if (is.numeric(yr)) {
  cy <- cy[year %in% yr]
}

# if data is already estimated.
force <- FALSE
by_vars <- c("country", "year", "reporting_level", "welfare_type")
if (!is.null(med) || force == FALSE) {
  cy <- cy[!med[, ..by_vars],
           on = by_vars]
}
setnames(cy, "median", "povline")

# note: make sure it works when povline has changed


# cy <- cy[1:10]
# cy <- cy[50:54]

if (nrow(cy) > 0) {

  prg_dir <- "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | rate [{cli::pb_rate}] | ETA: {cli::pb_eta}"

  ps_imp_povline <- purrr::possibly(implicit_povline, otherwise = NA)


  lmed <- purrr::pmap_dbl(cy,
                          ps_imp_povline,
                          lkup = lkup,
                          .progress = list(format = prg_dir))

  c_prob <- which(is.na(lmed))
  cy_prob <- cy[c_prob]
  cli::cli_alert_danger("The following country/year are problematic")
  cy_prob[]


  med2 <- add_vars(cy, median = lmed) |>
    fselect(country,
            year,
            reporting_level,
            welfare_type,
            median)

  if (!is.null(med)) {
    med <- rowbind(med, med2, fill = TRUE)
  } else {
    med <- copy(med2)
  }

  fst::write_fst(med, lup_medians_f)
}


pushoverr::pushover("Lined up medians DONE")



# plot(1:lmed[[i]]$iterations, lmed[[i]]$attempt, type = "o")


# lmed <- purrr::pmap(cy,
#                     implicit_povline,
#                     lkup = lkup,
#                     complete_return = TRUE,
#                     .progress = list(format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | rate [{cli::pb_rate}] | ETA: {cli::pb_eta}"))

# lmed <- purrr::pmap_dbl(cy,
#                         implicit_povline,
#                         lkup = lkup,
#                         .progress = list(format = " {cli::pb_bar} {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | rate [{cli::pb_rate}] | ETA: {cli::pb_eta}"))
#

