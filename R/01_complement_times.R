#' Create hourly timestamps between a given date end date
#'
#' @param min_date start date
#' @param max_date end date
#' @return data.frame with a date_time column
#'
#' @export
create_hourly_timestamps <- function(min_date, max_date) {
  stopifnot(min_date <= max_date)
  data.frame(date_time = seq(min_date, max_date, by = 'hour'))
}
