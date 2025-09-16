
# setup ---------
# source("R/setup.R")


# Functions -------------

filter_data <- function(country, year, welfare_type, dt_pop, version) {

  # load cache data --------------
  dt <-  load_cache(country, year, welfare_type, version)

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




load_cache <- function(country, year, welfare_type, version) {
  ### load welfare data ------
  WT <-
    welfare_type |>
    substr(1, 3) |>
    toupper()

  dt <-
    pipload::pip_load_cache(country      = country,
                            year         = year,
                            welfare_type = WT,
                            verbose      = FALSE,
                            version      = version)

}



get_md_median <- function(country, year, welfare_type, version) {
  dt <- load_cache(country, year, welfare_type, version)

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
#' @param first_delta numeric: first jump. Default 3
#' @inheritDotParams pipapi::pip
#'
#' @return numeric value
#' @export
#'
#' @examples
implicit_povline <- function(goal            = 0.5,
                             povline         =  2.15,
                             country         = "AGO",
                             year            = 1987,
                             reporting_level = "urban",
                             fill_gaps       = TRUE,
                             welfare_type    = "all",
                             lkup,
                             complete_return = FALSE,
                             tolerance       = 4,
                             ni              = 40,
                             first_delta     = 3
                             ) {

  # initial parameters -----------

  s          <- 0    # iteration stage counter
  num        <- 1    # numerator
  i          <- 1    # general counter
  status     <- "OK"


  #   main calculations ----------
  ## recording vectors --------------

  attempt <- delta <- pl <- vector("double", length = ni)
  delta[i] <- first_delta
  pl[i]    <- povline


  ## First call ---------
  # cli::cli_progress_bar(format = "{country}-{year}")
  attempt[i] <- pip_call(povline = pl[i],
                         lkup    = lkup,
                         country = country,
                         year    = year,
                         reporting_level = reporting_level,
                         fill_gaps = fill_gaps,
                         welfare_type = welfare_type)

  # attempt <- pip_call(povline = pl,
  #                     country = "AGO",
  #                     year    = 1987,
  #                     fill_gaps = TRUE,
  #                     reporting_level = "urban",
  #                     lkup = lkup)

  ## in case there is no data for requested year---------

  if (length(attempt[i]) == 0) {
    s          <- ni + 1 # avoid the while loop
    attempt[i] <- 0
    goal       <-  NA
    pl[i]      <-  NA
    status     <- "No data"
  }


  #   start looping -------------


  while (!identical(round(attempt[i],tolerance), goal) && i < ni) {
    i <-  i + 1

    jump <- delta[i - 1]
    if (attempt[i - 1] < goal) {
      # before crossing goal
      while (pl[i - 1] + jump < 0) {
        jump <- jump * 2
      }
      pl[i] <- pl[i - 1] + jump
      below <- 1
    }

    if (attempt[i - 1] > goal) {
      # after crossing goal
      while (pl[i - 1] - jump < 0) {
        jump <- jump / 2
      }
      pl[i] <- pl[i - 1] - jump
      below <-  0
    }


    # call data
    attempt[i] <- pip_call(povline = pl[i],
                           lkup    = lkup,
                           country = country,
                           year    = year,
                           reporting_level = reporting_level,
                           fill_gaps = fill_gaps,
                           welfare_type = welfare_type)

    # assess if the value of delta has to change
    if ((attempt[i] > goal & below == 1) |
        (attempt[i] < goal & below == 0)) {
      s <- s + 1

      if (!identical(s %% 2, 0)) {
        one <- -1
      } else {
        one <-  1
      }

      num <- (2 * num) + one
      den <- 2 ^ s
      delta[i] <- (num / den) * jump

    } else {
      delta[i] <- jump
    }  # end of condition to change the value of delta


    # cli::cli_progress_update()
  }  # end of while
  # cli::cli_progress_update(force = TRUE)

  if (complete_return) {
   list(attempt = attempt[1:i],
        delta   = delta[1:i],
        povline = pl[1:i],
        final   = pl[i],
        iterations = i)
  } else {
    return(pl[i])
  }


}  # End of function povcalnet_iterate


pip_call <- function(povline, lkup, ...) {
  pipapi::pip(povline = povline,
              lkup    = lkup,
                   ...) |>
    fselect(headcount) |>
    reg_elem()
}





get_median_from_synth_vec <- \(country, year, welfare_type, dt_pop, version) {
  # load group data and filter mean and population which are already loaded in
  # the global env
  lt <- filter_data(country, year, welfare_type, dt_pop, version)
  # get synthetic vector for any reporting level available
  sth <- get_synth_vecs(lt)
  # estimate median at the national level
  med <- sth |>
    fsummarise(median = fmedian(welfare, w = weight))
  med
}
