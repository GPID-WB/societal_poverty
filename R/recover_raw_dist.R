# here we should have a code tha replaces the following lines in the lineup_years.R files

## loau data --------
dt <- fst::read_fst(fs::path(new_dir,
                             "raw_recovered_dist", ext = "fst"),
                    as.data.table = TRUE)
#
