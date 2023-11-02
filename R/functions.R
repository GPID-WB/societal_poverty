
# setup ---------
source("R/setup.R")


# Functions -------------

filter_data <- function(country, year, welfare_type, dt_pop) {

  # load cache data --------------
  dt <-  load_cache(country, year, welfare_type)

  ### Load pop data ---------------
  yr <- year # to make it work
  pop <-
    dt_pop |>
    fsubset(country_code == country &
              year       == yr) |>
    fselect(reporting_level = pop_data_level,
            pop)

  ### get mean -------------

  mn <- svy |>
    fsubset(country_code   == country &
              reporting_year == year &
              welfare_type   == welfare_type)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(data = dt,
              pop  = pop,
              mean  = mn))
}
get_synth_vecs <- function(lt) {


  reporting_levels <-
    lt$data$reporting_level |>
    funique()

  sth <- lapply(reporting_levels,
                \(rl) {
                  welfare    <- lt$data[reporting_level == rl,
                                        welfare]
                  population <- lt$data[reporting_level == rl,
                                        weight]
                  mean       <-
                    lt$mean |>
                    fsubset(reporting_level == rl) |>
                    fselect(survey_mean_ppp) |>
                    as.numeric()

                  pop <- lt$pop |>
                    fsubset(reporting_level == rl) |>
                    fselect(pop) |>
                    as.numeric()

                  wbpip:::sd_create_synth_vector(welfare    = welfare,
                                                 population = population,
                                                 mean       = mean,
                                                 pop        = pop)
                }) |>
    rbindlist()

}




load_cache <- function(country, year, welfare_type) {
  ### load welfare data ------
  WT <-
    welfare_type |>
    substr(1, 3) |>
    toupper()

  dt <-
    pipload::pip_load_cache(country      = country,
                            year         = year,
                            welfare_type = WT,
                            verbose      = FALSE)

}



get_md_median <- function(country, year, welfare_type) {
  dt <- load_cache(country, year, welfare_type)

  med <- dt |>
    fgroup_by(imputation_id) |>
    fsummarise(median = fmedian(welfare_ppp, w = weight)) |>
    fselect(median) |>
    fmean()

  return(med)
}







#' implicit poverty line
#'
#' @param goal numeric: population share to get implicit line for. Must be
#'   between 0 and 1. Default is .5
#' @param pl numeric: initial poverty line. Default is 2.15
#' @param tolerance numeric: decimal precission. Default is 5 decimal places
#'   decimals
#' @param ni numeric: number of iterations before converging. Default 40.
#' @param delta numeric: first jump. Default 3
#' @inheritDotParams pipapi::pip
#'
#' @return numeric value
#' @export
#'
#' @examples
implicit_povline <- function(goal      = 0.5,
                             povline   =  2.15,
                             tolerance = 5,
                             ni        = 40,
                             delta     = 3,
                             ...) {

  # initial parameters -----------

  s          <- 0    # iteration stage counter
  num        <- 1    # numerator
  i          <- 0    # general counter
  status     <- "OK"
  pl         <- povline


  #   main calculations ----------


  ## First call ---------
  attempt <- pip_call(povline = pl, ...)

  ## in case there is no data for requested year---------

  if (length(attempt) == 0) {
    s       <- ni + 1 # avoid the while loop
    attempt <- 0
    goal    <-  NA
    pl      <-  NA
    status  <- "No data"
  }


  #   start looping -------------


  while (identical(round(attempt,tolerance), goal) != goal && s < ni) {
    i <-  i + 1

    if (attempt < goal) {
      # before crossing goal
      while (pl + delta < 0) {
        delta <-  delta * 2
      }
      pl <- pl + delta
      below <- 1
    }

    if (attempt > goal) {
      # after crossing goal
      while (pl - delta < 0) {
        delta <- delta / 2
      }
      pl <- pl - delta
      below <-  0
    }

    # call data
    attempt <- pip_call(povline = pl, ...)

    # assess if the value of delta has to change
    if ((attempt > goal & below == 1) |
        (attempt < goal & below == 0)) {
      s <- s + 1

      if (!identical(s %% 2, 0)) {
        one <- -1
      } else {
        one <-  1
      }

      num <- (2 * num) + one
      den <- 2 ^ s
      delta <- (num / den) * delta

    }  # end of condition to change the value of delta
  }  # end of while


  return(pl)


}  # End of function povcalnet_iterate


pip_call <- function(povline, ...) {
  pip <- pipapi::pip(povline = povline,
                         ...) |>
    qDT()

  pip[, headcount]
}

