test_that("get_years(): Convert Dates to Vector of Years correctly", {
  correct_date <-
    get_years(
      as.POSIXct("2012-01-01 00:00:00 Europe/Berlin"),
      as.POSIXct("2030-12-31 23:00:00 Europe/Berlin")
    )
  expect_equal(correct_date, 2011:2030)
})

test_that("get_years(): end date before start date", {
  expect_error(
    get_years(
      as.POSIXct("2021-01-01 00:00:00 Europe/Berlin"),
      as.POSIXct("2012-12-31 23:00:00 Europe/Berlin")),
    regexp = "The start date must begin before the end date!"
  )
})

test_that("filter_long_weekend(): Filtering of long weekends is working!",
          {
            df_input <-
              data.frame(
                type_school_holiday = c("Complicated Test Holiday",
                                        "Normal Holiday"),
                date_start_end = c(
                  "03.02.+18.05.+25.05.-02.05.+04.05.-06.07.",
                  "07.09.-15.09."
                )
              )
            df_expected <-
              data.frame(
                type_school_holiday = c("Normal Holiday",
                                        rep("Complicated Test Holiday", 4)),
                date_start_end = c(
                  "07.09.-15.09.",
                  "03.02.",
                  "18.05.",
                  "25.05.-02.05.",
                  "04.05.-06.07."
                )
              )
            expect_equal(filter_long_weekend(df_input), df_expected)
          })

test_that("interval_to_hourly_sequence(): Conversion to Time Interval is working!",
          {
            df_input <-
              data.frame(
                type_school_holiday = c("Holiday A", "Holiday B"),
                date_start = c("2000-01-01", "2010-10-11"),
                date_end = c("2000-01-27", "2010-10-19")
              )
            expect_equal(nrow(interval_to_hourly_sequence(df_input)),
                         27 * 24 + 9 * 24)
          })

test_that("separate_date_interval(): Separation of Date Intervals is working!",
          {
            df_input <-
              data.frame(
                type_school_holiday = c("A", "B", "C"),
                date_start_end = c("01.01.-10.01.",
                                   "02.02.-20.02.",
                                   "03.03.-30.03.")
            )
            df_expected <-
              data.frame(
                type_school_holiday = c("A", "B", "C"),
                date_start = c("01.01.",
                               "02.02.",
                               "03.03."),
                date_end = c("10.01.",
                             "20.02.",
                             "30.03.")
            )
            expect_equal(separate_date_interval(df_input), df_expected)
          })

test_that("add_year_to_date(): Adding year to date is working for normal holiday!",
          {
            df_input <-
              data.frame(
                type_school_holiday = c("A", "B"),
                date_start = c("01.01.",
                               "02.02."),
                date_end = c("10.01.",
                             "20.02.")
            )
            df_expected <-
              data.frame(
                type_school_holiday = c("A", "B"),
                date_start = c("01.01.1970",
                               "02.02.1970") %>% as.Date(format = "%d.%m.%Y"),
                date_end = c("10.01.1970",
                             "20.02.1970") %>% as.Date(format = "%d.%m.%Y")
            )
            expect_equal(add_year_to_date(df_input, 1970), df_expected)
          })

test_that("add_year_to_date(): Adding year to date is working for christmas holiday!",
          {
            df_input <-
              data.frame(
                type_school_holiday = c("Weihnachtsferien"),
                date_start = c("01.12."),
                date_end = c("31.01.")
              )
            df_expected <-
              data.frame(
                type_school_holiday = c("Weihnachtsferien"),
                date_start = c("01.12.1970") %>% as.Date(format = "%d.%m.%Y"),
                date_end = c("31.01.1971") %>% as.Date(format = "%d.%m.%Y")
              )
            expect_equal(add_year_to_date(df_input, 1970), df_expected)
          })
