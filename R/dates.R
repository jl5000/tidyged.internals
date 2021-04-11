
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

