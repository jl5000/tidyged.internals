
#' Return the current date in DATE_EXACT format
#'
#' @return The current date in DATE_EXACT format.
#' @export
date_current <- function() { 
  current_date <- Sys.Date()
  date_exact(lubridate::day(current_date),
             lubridate::month(current_date),
             lubridate::year(current_date))
}


#' Construct a DATE_EXACT string
#' 
#' @details Even though the day, month, and year are all required for an exact date, empty default
#' values have been set so that they are represented as zero-length rows in the tidyged file (i.e.
#' omitted).
#'
#' @param day The day of the month.
#' @param month The month of the year.
#' @param year The year.
#' @tests
#' expect_equal(date_exact(12), character())
#' expect_equal(date_exact(12, 8), character())
#' expect_equal(date_exact(12, 8, 2005), "12 AUG 2005")
#' @return A DATE_EXACT string
#' @export
date_exact <- function(day = numeric(), 
                       month = numeric(), 
                       year = numeric()) {
  
  if (length(day) + length(month) + length(year) < 3) return(character())
  
  validate_date(year, month, day)
  
  paste(day, toupper(month.abb[month]), year)
  
}


#' Construct a DATE_VALUE string
#'
#' @details The DATE_VALUE describes either a specific date, a date range, or a date period.
#' 
#' Dates can be qualified with the about, calc, and est parameters.
#' Date ranges can be expressed with the before, after, and between parameters.
#' Date periods can be expressed with the from and to parameters.
#' 
#' Either one or two dates can be provided depending on the date type. The following combinations of
#' date components are allowed: Y, YM, YMD, MD. Anything different will raise an error.
#' 
#' @param start_year The year (or start year if a date range/period is provided).
#' @param start_month The month of the year (or start month if a date range/period is provided).
#' @param start_day The day of the month (or start day if a date range/period is provided).
#' @param end_year The year of the end date.
#' @param end_month The month of the end date.
#' @param end_day The day of the end date.
#' @param from Whether the date given by start_year/month/day should be considered the beginning of a period.
#' If 'to' is FALSE then the period is semi-infinite.
#' @param to Whether the date given by end_year/month/day should be considered the end of a period.
#' If 'from' is FALSE then the period is semi-infinite.
#' @param before Whether the date given by start_year/month/day should be considered the end of a range.
#' @param after Whether the date given by start_year/month/day should be considered the beginning of a range.
#' @param between Whether the start and end dates should be considered as the bounds of a date range.
#' @param about Whether the date given by start_year/month/day is approximate.
#' @param calc Whether the date given by start_year/month/day is calculated from other values.
#' @param est Whether the date given by start_year/month/day is estimated.
#' @param start_year_is_bce If the start year is given without a day or month, whether it should
#' be interpreted as being before the Common Era.
#' @param end_year_is_bce If the end year is given without a day or month, whether it should
#' be interpreted as being before the Common Era.
#' @param start_year_is_dual If the start year is given with a month, whether to interpret the
#' start year as the first part of a dual year (only for English dates pre-1752). If TRUE, it will
#' transform a year of 1745 to 1745/46.
#' @param end_year_is_dual If the end year is given with a month, whether to interpret the
#' end year as the first part of a dual year (only for English dates pre-1752). If TRUE, it will
#' transform a year of 1745 to 1745/46.
#' @tests
#' expect_equal(date_value(2005), "2005")
#' expect_equal(date_value(2005, start_year_is_bce = TRUE), "2005 BCE")
#' expect_equal(date_value(2005, 1), "JAN 2005")
#' expect_equal(date_value(2005, 1, 14), "14 JAN 2005")
#' expect_equal(date_value(2005, after = TRUE), "AFT 2005")
#' expect_equal(date_value(2010, before = TRUE), "BEF 2010")
#' expect_equal(date_value(2005, 10, 14, before = TRUE), "BEF 14 OCT 2005")
#' expect_equal(date_value(1900, 6, 30, from = TRUE), "FROM 30 JUN 1900")
#' expect_equal(date_value(2000, 1, 1, to = TRUE), "TO 1 JAN 2000")
#' expect_equal(date_value(2005, 1, 14, 2006, 7, 9, from = TRUE, to = TRUE), "FROM 14 JAN 2005 TO 9 JUL 2006")
#' expect_equal(date_value(2005, 1, 14, 2006, 7, 9, between = TRUE), "BET 14 JAN 2005 AND 9 JUL 2006")
#' expect_equal(date_value(2005, 1, 14, calc = TRUE), "CAL 14 JAN 2005")
#' expect_equal(date_value(2005, est = TRUE), "EST 2005")
#' expect_equal(date_value(2005, 1, about = TRUE), "ABT JAN 2005")
#' @return A DATE_VALUE string
#' @export
date_value <- function(start_year = numeric(),
                       start_month = numeric(),
                       start_day = numeric(),
                       end_year = numeric(),
                       end_month = numeric(),
                       end_day = numeric(),
                       from = FALSE,
                       to = FALSE,
                       before = FALSE,
                       after = FALSE,
                       between = FALSE,
                       about = FALSE,
                       calc = FALSE,
                       est = FALSE,
                       start_year_is_bce = FALSE,
                       end_year_is_bce = FALSE,
                       start_year_is_dual = FALSE,
                       end_year_is_dual = FALSE) {
  
  if(length(start_year) + length(start_month) == 0) return(character())
  
  if(!start_year_is_bce & !end_year_is_bce)
    validate_date(start_year, start_month, start_day, end_year, end_month, end_day)
  
  val <- ""
  if (from) val <- "FROM"
  if (to & length(end_year) == 0) val <- "TO"
  
  if (before) {
    val <- "BEF"
  } else if (after) {
    val <- "AFT"
  } else if (between) {
    val <- "BET"
  } else if (about) {
    val <- "ABT"
  } else if (calc) {
    val <- "CAL"
  } else if (est) {
    val <- "EST"
  }
  
  if (length(start_day) == 1) val <- paste(val, start_day)
  if (length(start_month) == 1) val <- paste(val, toupper(month.abb[start_month]))
  if (length(start_year) == 1) {
    val <- paste(val, start_year)
    if(length(start_month) + length(start_day) == 0 & start_year_is_bce)
      val <- paste(val, "BCE")
    if(length(start_month) == 1 & start_year_is_dual) {
      next_year <- start_year + 1
      val <- paste0(val, "/", stringr::str_sub(next_year, -2))
    }
  }
  
  if (length(end_year) == 1) {
    if (to) {
      val <- paste(val, "TO")
    } else if (between) {
      val <- paste(val, "AND")
    }
    
    if (length(end_day) == 1) val <- paste(val, end_day)
    if (length(end_month) == 1) val <- paste(val, toupper(month.abb[end_month]))
    val <- paste(val, end_year)
    if(length(end_month) + length(end_day) == 0 & end_year_is_bce)
      val <- paste(val, "BCE")
    if(length(end_month) == 1 & end_year_is_dual) {
      next_year <- end_year + 1
      val <- paste0(val, "/", stringr::str_sub(next_year, -2))
    }
  }
  
  stringr::str_trim(val)
}


#' Construct a DATE_PERIOD string
#'
#' @param start_year The start year of the period. This value is required.
#' @param start_month The start month of the period.
#' @param start_day The start day of the period.
#' @param end_year The end year of the period
#' @param end_month The end month of the period.
#' @param end_day The end day of the period.
#' @param to Whether the date given by start_year/month/day should be considered the end of a period.
#' @param start_year_is_bce If the start year is given without a day or month, whether it should
#' be interpreted as being before the Common Era.
#' @param end_year_is_bce If the end year is given without a day or month, whether it should
#' be interpreted as being before the Common Era.
#' @param start_year_is_dual If the start year is given with a month, whether to interpret the
#' start year as the first part of a dual year (only for English dates pre-1752). If TRUE, it will
#' transform a year of 1745 to 1745/46.
#' @param end_year_is_dual If the end year is given with a month, whether to interpret the
#' end year as the first part of a dual year (only for English dates pre-1752). If TRUE, it will
#' transform a year of 1745 to 1745/46.
#' @tests
#' expect_equal(date_period(2005), "FROM 2005")
#' expect_equal(date_period(2005, 1), "FROM JAN 2005")
#' expect_equal(date_period(2005, 1, 14), "FROM 14 JAN 2005")
#' expect_equal(date_period(2005, to = TRUE), "TO 2005")
#' expect_equal(date_period(2010, 6, to = TRUE), "TO JUN 2010")
#' expect_equal(date_period(2005, 10, 14, 2008, 9), "FROM 14 OCT 2005 TO SEP 2008")
#' expect_equal(date_period(2005, 10, 14, 2008, 9, start_year_is_dual = TRUE), 
#'              "FROM 14 OCT 2005/06 TO SEP 2008")
#' expect_equal(date_period(2005, 10, 14, 2008, 9, end_year_is_dual = TRUE), 
#'              "FROM 14 OCT 2005 TO SEP 2008/09")
#' expect_equal(date_period(1900, 6, 30, 1901), "FROM 30 JUN 1900 TO 1901")
#' @return A DATE_PERIOD string
#' @export
date_period <- function(start_year = numeric(),
                        start_month = numeric(),
                        start_day = numeric(),
                        end_year = numeric(),
                        end_month = numeric(),
                        end_day = numeric(),
                        to = FALSE,
                        start_year_is_bce = FALSE,
                        end_year_is_bce = FALSE,
                        start_year_is_dual = FALSE,
                        end_year_is_dual = FALSE) {
  
  if(length(start_year) + length(start_month) == 0) return(character())
  
  if(!start_year_is_bce & !end_year_is_bce)
    validate_date(start_year, start_month, start_day, end_year, end_month, end_day)
  
  if (!to) { 
    val <- "FROM"
  } else {
    val <- "TO"
  }
  
  if (length(start_day) == 1) val <- paste(val, start_day)
  if (length(start_month) == 1) val <- paste(val, toupper(month.abb[start_month]))
  if (length(start_year) == 1) {
    val <- paste(val, start_year)
    if(length(start_month) + length(start_day) == 0 & start_year_is_bce)
      val <- paste(val, "BCE")
    if(length(start_month) == 1 & start_year_is_dual) {
      next_year <- start_year + 1
      val <- paste0(val, "/", stringr::str_sub(next_year, -2))
    }
  }
  
  if (length(end_year) == 1) {
    val <- paste(val, "TO")
    
    if (length(end_day) == 1) val <- paste(val, end_day)
    if (length(end_month) == 1) val <- paste(val, toupper(month.abb[end_month]))
    val <- paste(val, end_year)
    if(length(end_month) + length(end_day) == 0 & end_year_is_bce)
      val <- paste(val, "BCE")
    if(length(end_month) == 1 & end_year_is_dual) {
      next_year <- end_year + 1
      val <- paste0(val, "/", stringr::str_sub(next_year, -2))
    }
  }
  
  stringr::str_trim(val)
  
}
