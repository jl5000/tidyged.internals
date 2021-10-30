
#' Return the current date in DATE_EXACT format
#'
#' @return The current date in DATE_EXACT format.
#' @export
date_current <- function() { 
  current_date <- Sys.Date()
  date_exact(lubridate::year(current_date),
             lubridate::month(current_date),
             lubridate::day(current_date))
}


#' Construct a DATE_EXACT string
#' 
#' @details Even though the day, month, and year are all required for an exact date, empty default
#' values have been set so that they are represented as zero-length rows in the tidyged file (i.e.
#' omitted).
#'
#' @param year The year.
#' @param month The month of the year.
#' @param day The day of the month.
#' @tests
#' expect_equal(date_exact(2005), character())
#' expect_equal(date_exact(2005, 8), character())
#' expect_equal(date_exact(2005, 8, 12), "12 AUG 2005")
#' @return A DATE_EXACT string
#' @export
date_exact <- function(year = numeric(),
                       month = numeric(),
                       day = numeric()) {
  
  if (length(day) + length(month) + length(year) < 3) return(character())
  
  chk_date(year, month, day)
  
  paste(day, toupper(month.abb[month]), year)
  
}



#' Construct a DATE_CALENDAR string
#'
#' @param year The year.
#' @param month The month number.
#' @param day The day number.
#' @param year_is_bce If the year is given without a day or month, whether it should
#' be interpreted as being before the Common Era.
#' @param year_is_dual If the year is given with a month, whether to interpret the
#' year as the first part of a dual year (only for English dates pre-1752). If TRUE, it will
#' transform a year of 1745 to 1745/46.
#'
#' @return A DATE_CALENDAR string.
#' @export
#' @tests
#' expect_equal(date_calendar(2005), "2005")
#' expect_equal(date_calendar(103, year_is_bce = TRUE), "103 BCE")
#' expect_equal(date_calendar(2005, 1), "JAN 2005")
#' expect_equal(date_calendar(2005, 1, 14), "14 JAN 2005")
date_calendar <- function(year = numeric(), 
                          month = numeric(), 
                          day = numeric(),
                          year_is_bce = FALSE,
                          year_is_dual = FALSE) {
  
  if(length(year) + length(month) == 0) return(character())
  
  chk_date(year, month, day)
  
  val <- ""
  if (length(day) == 1) val <- paste(val, day)
  if (length(month) == 1) val <- paste(val, toupper(month.abb[month]))
  if (length(year) == 1) {
    val <- paste(val, year)
    if(length(month) + length(day) == 0 & year_is_bce)
      val <- paste(val, "BCE")
    if(length(month) == 1 & year_is_dual) {
      next_year <- year + 1
      val <- paste0(val, "/", stringr::str_sub(next_year, -2))
    }
  }
  stringr::str_trim(val)
}


#' Construct a DATE_RANGE string
#'
#' @param start_date A DATE_CALENDAR() object giving the start of the range. If only the
#' start date is provided, a string of the form "AFT date" will be returned.
#' @param end_date A DATE_CALENDAR() object giving the end of the range. If only the
#' end date is provided, a string of the form "BEF date" will be returned.
#'
#' @return A DATE_RANGE string.
#' @export
#'
#' @tests
#' expect_error(date_range("ABT 2008"))
#' expect_equal(date_range(start_date = date_calendar(2005)), 
#'              "AFT 2005")
#' expect_equal(date_range(end_date = date_calendar(2010)), 
#'              "BEF 2010")
#' expect_equal(date_range(end_date = date_calendar(2005, 10, 14)), 
#'              "BEF 14 OCT 2005")
#' expect_equal(date_range(date_calendar(2005, 1, 14), date_calendar(2006, 7, 9)), 
#'              "BET 14 JAN 2005 AND 9 JUL 2006")
#' expect_equal(date_range(), character())
date_range <- function(start_date = date_calendar(),
                       end_date = date_calendar()) {
  
  if((length(start_date) > 0 && !stringr::str_detect(start_date, reg_date_calendar())) |
     (length(end_date) > 0 && !stringr::str_detect(end_date, reg_date_calendar())))
    stop("Start and end dates must be date_calendar() objects")
    
  if (length(start_date) + length(end_date) == 2) {
    
    chk_dates(start_date, end_date)
    paste("BET", start_date, "AND", end_date)
    
  } else if (length(start_date) == 1) {
    
    paste("AFT", start_date)
    
  } else if (length(end_date) == 1) {
    
    paste("BEF", end_date)
    
  } else {
    character()
  }
  
}

#' Construct a DATE_PERIOD string
#'
#' @param start_date A DATE_CALENDAR() object giving the start of the period. If only the
#' start date is provided, a string of the form "FROM date" will be returned.
#' @param end_date A DATE_CALENDAR() object giving the end of the period. If only the
#' end date is provided, a string of the form "TO date" will be returned.
#'
#' @return A DATE_PERIOD string.
#' @export
#'
#' @tests
#' expect_error(date_period("ABT 2008"))
#' expect_equal(date_period(start_date = date_calendar(2005)), 
#'              "FROM 2005")
#' expect_equal(date_period(start_date = date_calendar(2005, 1)), 
#'              "FROM JAN 2005")
#' expect_equal(date_period(start_date = date_calendar(2005, 1, 14)), 
#'              "FROM 14 JAN 2005")
#' expect_equal(date_period(end_date = date_calendar(2005)), 
#'              "TO 2005")
#' expect_equal(date_period(end_date = date_calendar(2010, 6)), 
#'              "TO JUN 2010")
#' expect_equal(date_period(start_date = date_calendar(2005, 10, 14), end_date = date_calendar(2008, 9)), 
#'              "FROM 14 OCT 2005 TO SEP 2008")
#' expect_equal(date_period(start_date = date_calendar(1750, 10, year_is_dual = TRUE), 
#'                          end_date = date_calendar(2008, 9)), 
#'              "FROM OCT 1750/51 TO SEP 2008")
#' expect_equal(date_period(date_calendar(1900, 6, 30), date_calendar(1901)), 
#'              "FROM 30 JUN 1900 TO 1901")
date_period <- function(start_date = date_calendar(),
                       end_date = date_calendar()) {
  
  if((length(start_date) > 0 && !stringr::str_detect(start_date, reg_date_calendar())) |
     (length(end_date) > 0 && !stringr::str_detect(end_date, reg_date_calendar())))
    stop("Start and end dates must be date_calendar() objects")
  
  if (length(start_date) + length(end_date) == 2) {
    
    chk_dates(start_date, end_date)
    paste("FROM", start_date, "TO", end_date)
    
  } else if (length(start_date) == 1) {
    
    paste("FROM", start_date)
    
  } else if (length(end_date) == 1) {
    
    paste("TO", end_date)
    
  } else {
    character()
  }
  
}


#' Construct a DATE_APPROXIMATED string
#'
#' @param date A DATE_CALENDAR() object giving the uncertain date. 
#' @param about Whether the date is approximate.
#' @param calc Whether the date is calculated from other values.
#' @param est Whether the date is estimated.
#'
#' @return A DATE_APPROXIMATED string.
#' @export
#'
#' @tests
#' expect_equal(date_approximated(date_calendar(2005, 1, 14), calc = TRUE), "CAL 14 JAN 2005")
#' expect_equal(date_approximated(date_calendar(2005), est = TRUE), "EST 2005")
#' expect_equal(date_approximated(date_calendar(2005, 1)), "ABT JAN 2005")
#' expect_equal(date_approximated(), character())
#' expect_equal(date_approximated(date_calendar(2005, 1), FALSE,FALSE,FALSE), "JAN 2005")
date_approximated <- function(date = date_calendar(),
                              about = TRUE,
                              calc = FALSE,
                              est = FALSE) {
  
  if(length(date) == 0) return(character())
  
  if(calc) {
    paste("CAL", date)
  } else if(est) {
    paste("EST", date)
  } else if (about) {
    paste("ABT", date)
  } else {
    date
  }
  
}



#' Convert a GEDCOM date into a lubridate date
#'
#' @param date_string A date_calendar() string.
#' @param minimise Whether to fill in missing date pieces so that the date is minimised. For example, if no month is given, January is used. If minimise = FALSE, December will be used.
#'
#' @return A lubridate date.
#' @export
#' @tests
#' expect_equal(is.na(parse_gedcom_date(NA)), TRUE)
#' expect_equal(parse_gedcom_date("4 APR"), as.Date("1000-04-04"))
#' expect_equal(parse_gedcom_date("4 APR", minimise = FALSE), as.Date("4000-04-04"))
parse_gedcom_date <- function(date_string, minimise = TRUE) {
  if(is.na(date_string)) return(as.Date(NA))
  
  # remove dual year
  ged_date <- stringr::str_remove(date_string, "/\\d{2}") 
  
  if(stringr::str_detect(ged_date, "\\d{3,4}$")) {
    ged_year <- stringr::str_extract(ged_date, "\\d{3,4}$")
  } else {
    if(minimise) ged_year <- 1000 else ged_year <- 4000
  }
  
  if(stringr::str_detect(ged_date, "[A-Z]{3}")) {
    ged_month <- which(toupper(month.abb) == stringr::str_extract(ged_date, "[A-Z]{3}"))
  } else {
    if(minimise) ged_month <- 1 else ged_month <- 12
  }
 
  if(stringr::str_detect(ged_date, "^\\d{1,2} ")) {
    ged_day <- stringr::str_extract(ged_date, "^\\d{1,2} ") %>% stringr::str_trim()
  } else {
    if(minimise) {
      ged_day <- 1
    } else {
      ged_day <- lubridate::days_in_month(lubridate::make_date(ged_year, ged_month))
    }
  }
  
  lubridate::make_date(ged_year, ged_month, ged_day)
}


#' Convert a GEDCOM age at event into decimalised years
#'
#' @param age_string A string describing an age at an event, e.g. "14y 3m 20d".
#'
#' @return A numeric value giving the age in years.
#' @export
#' @tests
#' expect_equal(is.na(parse_gedcom_age(NA)), TRUE)
#' expect_equal(parse_gedcom_age("16y"), 16)
#' expect_equal(parse_gedcom_age("16y 6m"), 16.5)
#' expect_equal(parse_gedcom_age("73d"), 0.2)
parse_gedcom_age <- function(age_string) {
  if(is.na(age_string)) return(NA_real_)
  
  years <- stringr::str_extract(age_string, "\\d{1,3}y") %>% 
    stringr::str_replace("y", "")
  months <- stringr::str_extract(age_string, "\\d{1,2}m") %>% 
    stringr::str_replace("m", "")
  days <- stringr::str_extract(age_string, "\\d{1,3}d") %>% 
    stringr::str_replace("d", "")
  
  if(is.na(years)) years_num <- 0 else years_num <- as.numeric(years)
  if(is.na(months)) months_prop <- 0 else months_prop <- as.numeric(months)/12
  if(is.na(days)) days_prop <- 0 else days_prop <- as.numeric(days)/365
  
  years_num + months_prop + days_prop
  
}