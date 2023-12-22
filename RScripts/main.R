library(fairqFeaturesDatetime)
library(fairqDbtools)

# define time interval in POSIX
min_date <- as.POSIXct("2015-01-01 00:00:00 Europe/Berlin")
max_date <- as.POSIXct("2026-12-31 23:00:00 Europe/Berlin")

all_times <- create_hourly_timestamps(min_date, max_date)

times_with_school_holidays <-
  add_school_holidays(min_date, max_date, all_times)

times_with_public_holidays <-
  add_holidays(min_date, max_date, times_with_school_holidays)

send_data(times_with_public_holidays, "holidays", mode = "replace")
