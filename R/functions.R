
filter_data <- function(country, year, welfare_type, dt_pop) {

  ### load welfare data ------
  WT <-
    welfare_type |>
    substr(1, 3) |>
    toupper()

  dt <-
    pipload::pip_load_cache(country      = country,
                            year         = year,
                            welfare_type = WT)

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
