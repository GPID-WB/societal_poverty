# Load libraries -----------
library(fastverse)
set_collapse(
  nthreads = 4,
  sort = FALSE,
  mask = c("%in%"),
  remove = "old"
)
# Final conflicts check (optional)
fastverse_conflicts()

# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/pipapi@DEV")

# setup ------------

## directories -----------
force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)


version  <- "20240326_2011_02_02_PROD"
version  <- "20240326_2017_01_02_PROD"
version  <- "20240429_2017_01_02_INT"
version  <- "20240627_2017_01_02_PROD"


## Lkup files ===========
lkups <- pipapi::create_versioned_lkups(data_dir        = data_dir,
                                        vintage_pattern = version)
}

# ppp years
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()
# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <-  lkups$versions_paths[[lkups$latest_release]]


new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version)



## filter data --------------

yr <- 2024
ct <- "QAT"
ct <- NULL
yr <- NULL

## TFS dir -----------

gls <- pipfun::pip_create_globals(
  root_dir   = Sys.getenv("PIP_ROOT_DIR"),
  vintage    = version,
  create_dir = FALSE)
