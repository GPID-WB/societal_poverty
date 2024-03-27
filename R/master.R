# source("R/lineup_years.R")
source("R/svy_years.R")
msg <- paste("add spl", version, prettyNum(Sys.time()), sep = "<>")
ga()
gca(msg)
gp()
pushover("finish with SPR")

