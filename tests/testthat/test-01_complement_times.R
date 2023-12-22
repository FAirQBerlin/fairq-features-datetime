test_that("create_hourly_timestamps returns correct results", {
  expected1 <-
    data.frame(date_time = as.POSIXct(
      c(
        "2020-01-01 00:00:00",
        "2020-01-01 01:00:00",
        "2020-01-01 02:00:00"
      )
    ))
  res1 <-
    create_hourly_timestamps(as.POSIXct("2020-01-01 00:00:00"),
                             as.POSIXct("2020-01-01 02:00:00"))
  expect_equal(res1, expected1)

  res2 <-
    create_hourly_timestamps(as.POSIXct("2022-12-01 12:00:00"),
                             as.POSIXct("2022-12-03 12:00:00"))
  expect_equal(nrow(res2), 2 * 24 + 1)
})


test_that("create_hourly_timestamps throws error if min_date is after max_date",
          {
            expect_error(create_hourly_timestamps(
              as.POSIXct("2020-01-01 00:00:00"),
              as.POSIXct("1900-01-01 00:00:00")
            ),
            # Don't add complete error message because it depends on local language:
            "min_date <= max_date")
          })
