
Treatments <- data.table::data.table(unit = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11,
                                 12, 15, 18, 5, 7, 13, 14, 19, 24),
                        treatment = c(rep("light/no", times = 7),
                                      rep("med", times = 6),
                                      rep("heavy", times = 6)))
usethis::use_data(Treatments,overwrite = TRUE)


remeas_freq <- data.table::fread("./data-raw/summit_meas_interval.csv", na.strings = "n/a")
usethis::use_data(remeas_freq, overwrite = TRUE)
