#' Add information whether a day is a public holiday or not
#'
#' @param min_date (POSIXct) Start Date
#' @param max_date (POSIXct) End Date
#' @param all_times data.frame with a date_time column
#' @param min_date start date
#' @param max_date end date
#'
#' @return same data.frame with additional column "is_public_holiday" (0/1)
#' @export
add_holidays <- function(min_date, max_date, all_times) {
  date_time <- NULL # fix linting
  years <- get_years(min_date, max_date)
  holidays <- download_holidays(years)
  all_times %>%
    mutate(date = as.Date(date_time, tz = "Europe/Berlin")) %>%
    left_join(holidays, by = "date") %>%
    replace_na(list(is_public_holiday = as.integer(0))) %>%
    select(-c(date))
}

#' Download holidays for years 2015 to 2024
#'
#' @param years numeric vector with years for which we retrieve holidays
#'
#' @return data.frame with columns date and "is_public_holiday" (always 1)
download_holidays <- function(years) {
  holidays <- holiday(
    year = years,
    Holiday = c(
      "ChristmasDay", # 25/12
      "BoxingDay", # 26/12
      "NewYearsDay", # 01/01
      "GoodFriday", # "Karfreitag"
      "EasterMonday",
      "DEAscension", # "Christi Himmelfahrt"
      # "PentecostMonday", # "Pfingstmontag" - there seems to be a bug in the timeDate pkg
      "DEGermanUnity"
    )
  )
  holidays <- as.Date(holidays)

  # Add womens' day and first of May - not (yet) in the R package timeDate.
  # First women's day was in 2019.
  womens_day <- as.Date(paste0(2019:max(years), "-03-08"))
  first_of_may <- as.Date(paste0(years, "-05-01"))
  holidays <- c(holidays, womens_day, first_of_may)

  data.frame(date = holidays, is_public_holiday = as.integer(1))
}
