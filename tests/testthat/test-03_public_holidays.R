test_that("download_holidays returns correct results", {
  res <- download_holidays(years = 2019:2020)
  expect_equal(min(res$date), as.Date("2019-01-01"))
  expect_equal(max(res$date), as.Date("2020-12-26"))
  expect_equal(nrow(res), 18)
  expect_named(res, c("date", "is_public_holiday"))
  expect_true(all(res$is_public_holiday == 1))
})

test_that("add_holidays adds correct holidays", {
  input_df <- data.frame(
    date_time = c(
      # Christmas 2020, two different times:
      as.POSIXct("2020-12-25 01:00:00"),
      as.POSIXct("2020-12-25 02:00:00"),
      # No holiday:
      as.POSIXct("2020-12-12 12:00:00"),
      # First of May, 2021; this case also checks that time zone stuff works
      # correctly
      as.POSIXct("2021-05-01 00:00:00")
    ),
    another_columns = 1:4
  )
  res <-
    add_holidays(
      min_date = as.POSIXct("2015-01-01 00:00:00 Europe/Berlin"),
      max_date = as.POSIXct("2024-12-31 23:00:00 Europe/Berlin"),
      input_df
    )
  expected <- data.frame(
    date_time = c(
      as.POSIXct("2020-12-25 01:00:00"),
      as.POSIXct("2020-12-25 02:00:00"),
      as.POSIXct("2020-12-12 12:00:00"),
      as.POSIXct("2021-05-01 00:00:00")
    ),
    another_columns = 1:4,
    is_public_holiday = c(1, 1, 0, 1)
  )
  expect_equal(res, expected)
})
