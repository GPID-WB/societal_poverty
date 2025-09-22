# setup ---------
source("R/setup.R")

## read functions --------------
source("R/functions.R")
force <- FALSE
lup_medians_f <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE) |>
  fs::path("lup_medians", ext = "fst")

if (!fs::file_exists(lup_medians_f) || isTRUE(force)) {
  dt <- pipapi::pip(popshare = .5,
                    fill_gaps = TRUE,
                    lkup = lkup)
  med <- get_vars(dt, c("country_code",
                        "reporting_year",
                        "reporting_level",
                        "welfare_type",
                        "poverty_line")) |>
    frename(median = poverty_line)

  fst::write_fst(med, lup_medians_f)
} else {
  med <- fst::read_fst(lup_medians_f)
}


# Updsate manually a particualr country
manual <- FALSE
if (manual) {
  ctr <- "DOM"
  yr  <- 2024
  med <- pipapi::pip(country = ctr,
                     year = yr,
                     popshare = .5,
                     fill_gaps = TRUE,
                     lkup = lkup) |>
  get_vars(c("country_code",
                        "reporting_year",
                        "reporting_level",
                        "welfare_type",
                        "poverty_line")) |>
    frename(median = poverty_line)
}



## by vars ---------
by_vars <- c("country_code", "reporting_year", "reporting_level", "welfare_type")


# format data -----------

## filter and SPL ------
med2 <- med |>
  # fsubset(!(country %in% c("CHN", "IDN", "IND") &
  fsubset(!(country_code %in% c("CHN") &
              reporting_level != "national")) |>
  ftransform(spl = wbpip:::compute_spl(median, ppp_year))


# calculate SPR -------------

spl <- med2 |>
  fselect(country_code,
          reporting_level,
          spl,
          reporting_year,
          welfare_type) |>
  ftransform(spl = round(spl, 3))

povlines <- funique(spl$spl)


if (manual) {
  mpl <- pipapi::pip(country = ctr,
                     year = yr,
                     povline = povlines,
                     lkup = lkup,
                     fill_gaps = TRUE)

} else {
  mpl <- pipapi::pip(povline = povlines,
                     lkup = lkup,
                     fill_gaps = TRUE)
}

mpl <- get_vars(mpl, c("country_code",
                        "reporting_year",
                        "reporting_level",
                        "welfare_type",
                        "poverty_line",
                        "headcount")) |>
  funique() |>
  frename(poverty_line = spl)

# duplicate CHN
chnur <- spl[country_code == "CHN"
             ][rep(1:.N, each = 2)
              ][, g := rowid(reporting_year)
                ][,
                  reporting_level := fifelse(g == 1,
                                             "urban",
                                             "rural")
                  ][, g := NULL]

spl <- rowbind(spl, chnur, fill = TRUE) |>
  setorder(country_code, reporting_level, reporting_year) |>
  _[!is.na(country_code)]


spr <- join(spl, mpl,
            on = c("country_code",
                   "reporting_year",
                   "reporting_level",
                   "welfare_type",
                   "spl"),
            how = "left",
            validate = "1:1",
            overid = 2) |>
  setnames("headcount", "spr")



# join the median by reporting level
d_spr <- joyn::joyn(spr, med,
                    by = by_vars,
                    match_type = "1:1",
                    reportvar = FALSE)
setkey(d_spr, NULL)



# Load existing data if it exists -------------

## Project dir --------------
spl_datadir <-
  fs::path("data", version) |>
  fs::dir_create(recurse = TRUE)

fst_file <- fs::path(spl_datadir, "spr_lnp", ext = "fst")

if (fs::file_exists(fst_file)) {
  f_spr <- fst::read_fst(fst_file, as.data.table = TRUE)
  setkey(f_spr, NULL)
  byvars <- c("country_code", "reporting_year")

  # remove old  information
  f_spr <-  f_spr[!d_spr, on = byvars]

  # add new information
  d_spr <- rowbind(f_spr, d_spr, fill = TRUE)
}



# save ---------



fst::write_fst(d_spr, fst_file)
fst::write_fst(d_spr, fs::path(gls$OUT_AUX_DIR_PC, "spr_lnp", ext = "fst"))
fst::write_fst(d_spr, fs::path(lkup$data_root, "_aux", "spr_lnp", ext = "fst"))


haven::write_dta(d_spr, fs::path_ext_set(fst_file, "dta"))

spr_id <- joyn::is_id(d_spr,by_vars, return_report = TRUE )
spr_id[copies > 1]






