#' Add information whether a day is a school holiday or not
#'
#' @param min_date (POSIXct) start date
#' @param max_date (POSIXct) end date
#' @param all_times data.frame with a date_time column
#'
#' @return same data.frame with additional column "type_school_holiday"s
#' @export
add_school_holidays <- function(min_date, max_date, all_times) {
  school_holidays <- create_school_holidays(min_date, max_date)
  times_with_school_holidays <- all_times %>%
    left_join(school_holidays, by = "date_time")
  return(times_with_school_holidays)
}

#' Creates a dataframe with school holidays
#'
#' @description dataframe consists of equidistant hourly spaced "date_time" column
#' and "type_school_holiday" where the name of the holiday is stored.
#'
#' @param min_date (POSIXct) start date
#' @param max_date (POSIXct) end date
#'
#' @return data.frame with school holidays
create_school_holidays <- function(min_date, max_date) {
  years <- get_years(min_date, max_date)
  holidays_raw <-
    lapply(years, create_raw_school_holidays) %>% bind_rows() %>% arrange(.data$date_start)
  holidays <- interval_to_hourly_sequence(holidays_raw)
  return(holidays)
}


#' Create a data frame with dates of school holidays in a given year
#'
#' @description data frame consists of type of holiday, start date of the holiday,
#' end date of the holiday. Data is crawled from www.schulferien.org
#'
#' @param year (int) the year that should be crawled
#'
#' @return data.frame with start and end date of the holidays in given year
create_raw_school_holidays <- function(year) {
  State <- date_start_end <- NULL # fix linting

  dat <- get_data_schulferien_org(year)
  dat <- preprocess_data_schulferien(dat)
  # filter out and separate combined holidays
  if (any(grepl('\\+', dat$date_start_end))) {
    dat <- filter_long_weekend(dat)
  }
  dat <- separate_date_interval(dat)

  dat <- add_year_to_date(dat, year)
  return(na.omit(dat))
}

#' Web crawls schulferien.org
#'
#' @description crawls all German school holidays from schulferien.org of given year
#'
#' @param year (int) year to web crawl data
#'
#' @return data.frame with all school holidays of given year
get_data_schulferien_org <- function(year) {
  url <-
    paste0("https://www.schulferien.org/deutschland/ferien/",
           year,
           "/")
  dat <- read_html(url) %>% html_table(header = TRUE) %>% `[[`(1)
  return(dat)
}

#' Prepares Data from schulferien.org
#'
#' @description This function selects only the data of the federal State Berlin.
#' Converts the data frame from wide to long, removes all white spaces, asterisks and
#' drops all empty rows of data frame.
#'
#' @param dat (data.frame) data.frame given by schulferien.org
#'
#' @return processed data.frame
preprocess_data_schulferien <- function(dat) {
  State <- date_start_end <- NULL # fix linting

  # replacement of column name
  names(dat) <- dat[1, ] %>% as.character
  names(dat)[1] <- "State"
  # remove NA columns
  na_indices <- which(colnames(dat) == "NA")
  dat <- dat[, -na_indices]
  # only select Berlin
  dat <- dat %>% filter(grepl("Berlin", State)) %>% select(-State)
  # convert wide to long format transformation
  dat <- dat %>%
    gather(
      "type_school_holiday",
      "date_start_end",
      .data$Winterferien:.data$Weihnachtsferien
    )

  # remove all white spaces
  dat <-
    as.data.frame(apply(dat, 2, function(x)
      gsub("\\s+", "", x)))

  # remove all asterisks
  dat <-
    as.data.frame(apply(dat, 2, function(x)
      gsub("\\*", "", x)))

  # drop all rows with empty holidays
  dat <- dat %>% subset(date_start_end != "-")
  return(dat)
}


#' Filters out elements of data frame with "long weekends"
#'
#' @description Some dates of school holidays are not given by an
#' closed interval. They are given by a string with separators. The char "+"
#' separates single days, the char "-" indicates an interval.
#' e.g. "09.07.+10.07.-22.08."
#' This function separates these and stores them in multiple rows of the given data.frame
#'
#' @param dat (data.frame) with school holiday with combined "long weekend"
#'
#' @return processed data.frame with separate "long weekends"
#'
filter_long_weekend <- function(dat) {
  date_start_end <- NULL # fix linting
  # filter out every holiday with substring "+" and save in temporary data frame
  dat_tmp <- dat %>% filter(grepl('\\+', date_start_end))
  # remove of all rows with substring "+"
  dat <-
    dat %>% anti_join(y = dat_tmp,
                      by = c("type_school_holiday", "date_start_end"))
  # count the numbers of plus signs for each row
  n_plus <- str_count(dat_tmp$date_start_end, "\\+")
  # splitted time intervals
  splitted <- unlist(str_split(dat_tmp$date_start_end, "\\+"))
  # duplicate each row holiday by the number of time intervals
  dat_tmp <- dat_tmp[rep(1:nrow(dat_tmp), (n_plus + 1)), ]
  # insert splitted time interval
  dat_tmp[[2]] <- splitted
  # join both data frame
  dat <-
    full_join(dat,
              dat_tmp,
              by = c("type_school_holiday", "date_start_end"))

  return(dat)
}

#' Stores beginning and end of holidays in 2 separate variables
#'
#' @description This function separates an date interval in start
#' and end and replaces them in the given data.frame
#'
#' @param dat (data.frame) with combined date interval
#'
#' @return data.frame with separated start and end date
#'
separate_date_interval <- function(dat) {
  # create date_start and date_end columns
  dat <- dat %>%
    separate(
      .data$date_start_end,
      into = c("date_start", "date_end"),
      sep = "-",
      fill = "right"
    )

  # store beginning of holiday in date_start
  dat$date_start <- dat$date_start %>%
    substr(nchar(.) - 5, nchar(.)) %>%
    gsub("-", NA, .)

  # store end of holiday in date_end
  dat$date_end <- dat$date_end %>%
    substr(., 1, 6) %>%
    ifelse(!is.na(dat$date_start) & is.na(.),
           dat$date_start,
           .)
  return(dat)
}

#' Add year to date
#'
#' @description This function adds the given year to a given date which
#' only consist out of month and day
#'
#' @param dat (data.frame) with dates
#' @param year (int)
#'
#' @return data.frame completed with given year
#'
add_year_to_date <- function(dat, year) {
  # if Christmas holidays end in January, set current year to next year
  dat$date_start <-
    ifelse(is.na(dat$date_start), NA, paste0(dat$date_start, year)) %>%
    as.Date(format = "%d.%m.%Y")


  # To Do: ifelse in case_when
  dat$date_end <- ifelse(is.na(dat$date_end), NA,
                         paste0(
                           dat$date_end,
                           ifelse(
                             dat$type_school_holiday == "Weihnachtsferien" &
                               substr(dat$date_end, 4, 5) == "01",
                             year + 1,
                             year
                           )
                         )) %>%
    as.Date(format = "%d.%m.%Y")
  return(dat)
}

#' get years from date
#'
#' @description This function calculates a vector with the years from a start and end date
#' The start year is the year before the start date to make sure that Christmas
#' holidays are completely included.
#'
#' @param min_date (POSIXct) Start Date
#' @param max_date (POSIXct) End Date
#'
#' @return int vector containing all years
#'
get_years <- function(min_date, max_date) {
  if ((min_date <= max_date) != 1)
    stop("The start date must begin before the end date!")
  min_year <- as.integer(format(min_date, format = "%Y")) - 1
  max_year <- as.integer(format(max_date, format = "%Y"))
  years <- min_year:max_year
  return(years)
}

#' Convert a data frame with start and end date to a data frame with an hourly
#' spaced time interval
#'
#' @description This function transforms a given data frame with the holiday
#' name, start date and end date a data frame with same event name but with an
#' hourly spaced time interval.
#'
#' @param df (data.frame) with 3 Columns.
#' 1st Column - type_school_holiday,
#' 2nd Column - date_start,
#' 3rd Column - date_end
#'
#' @return df_new (data.frame) with 2 Columns
#' 1st Column - Event Name,
#' 2nd Column date_time with an hourly time stamp
interval_to_hourly_sequence <- function(df) {
  # Initialize Dataframe
  df_new <-
    tibble(date_time = POSIXct(), type_school_holiday = character())

  # for every row in df add date_time (every hour) and type of holiday
  for (row in 1:nrow(df)) {
    # start and end time of school holiday
    start <-
      as.POSIXct(paste(df$date_start[row], "00:00:00 Europe/Berlin"))
    end <-
      as.POSIXct(paste(df$date_end[row], "23:00:00 Europe/Berlin"))
    # vector of every hour of a school holiday
    df_new <- rbind(
      df_new,
      data.frame(
        date_time = create_hourly_timestamps(start, end),
        type_school_holiday = df$type_school_holiday[row]
      )
    )
  }
  return(df_new)
}
