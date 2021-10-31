
parse_error <- function(msg) {
  if(!is.null(msg)) stop(msg[1])
}

#' @tests
#' expect_error(chk_input_size(1:2, 1) %>% parse_error())
#' expect_error(chk_input_size("123456", 1, min_char = 7) %>% parse_error())
#' expect_error(chk_input_size("123456", 1, max_char = 5) %>% parse_error())
chk_input_size <- function(input, max_dim, min_char = NULL, max_char = NULL) {
  if (length(input) > max_dim)
    return(paste0("Input ", input[1], "... has too many dimensions. The limit is ", max_dim))
  
  if (length(input) > 0 && !is.null(max_char) && max(nchar(input)) > max_char)
    return(paste("Input", input[which.max(nchar(input))], "has too many characters. The limit is", max_char))
  
  if (length(input) > 0 && !is.null(min_char) && min(nchar(input)) < min_char)
    return(paste("Input", input[which.min(nchar(input))], "has too few characters. The minimum is", min_char))
  
  NULL
}

#' @tests
#' expect_error(chk_input_pattern("Test string", "Tast string") %>% parse_error())
chk_input_pattern <- function(input, pattern) {
  if (length(input) > 0) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(paste("Input", i, "is in an unexpected format"))
    }
  }
  NULL
}

#' @tests
#' expect_error(chk_input_choice(20, 22:28) %>% parse_error())
chk_input_choice <- function(input, choices) {
  if (length(input) == 1 && !input %in% choices) 
    return(paste("Invalid argument value:", input, "\n  The valid values are:", 
         paste(choices, collapse = ", ")))
  NULL
}


#' @tests
#' expect_error(chk_date(2005, day = 15) %>% parse_error())
#' expect_error(chk_date(month = 5) %>% parse_error())
#' expect_error(chk_date(2005, 13) %>% parse_error())
#' expect_error(chk_date(2005, 10, 32) %>% parse_error())
#' expect_error(chk_date(2005, -1, 6) %>% parse_error())
#' expect_error(chk_date(month = 1, day = 32) %>% parse_error())
chk_date <- function(year = integer(),
                          month = integer(),
                          day = integer()) {
  
  if (length(year) + length(day) < length(month))
    return("Month is defined without a day or year")
  
  if (length(month) < length(day))
    return("Day is defined without a month")
  
  # Set empties to something reasonable
  if (length(year) == 0) year <- 2000
  if (length(month) == 0) month <- 1
  if (length(day) == 0) day <- 1
  
  #Let lubridate do the heavy lifting
  test_date <- lubridate::make_date(year, month, day)
  if (is.na(test_date)) return("Date is invalid")
  NULL
}



#' Check one date_calendar() object occurs after another
#'
#' @param start_date A date_calendar() string for the earlier date.
#' @param end_date A date_calendar() string for the later date.
#'
#' @return Either a single character string describing the error encountered, or
#' NULL if no errors are found.
#' @tests
#' expect_error(chk_dates("18 MAY 2005", "17 MAY 2005") %>% parse_error())
#' expect_error(chk_dates("MAR 2005", "FEB 2004") %>% parse_error())
#' expect_error(chk_dates("2005", "2004") %>% parse_error())
chk_dates <- function(start_date, end_date) {

  date1 <- parse_gedcom_date(start_date, minimise = TRUE)
  date2 <- parse_gedcom_date(end_date, minimise = FALSE)

  if (date1 > date2) return("First date is after second date")
  NULL
}


#' Validate a tidyged input value
#' 
#' These functions check values for length, character limit, and form.
#' 
#' @details The functions are designed to be combined with the parse_error function, but are 
#' also used for input validation in the shinyged package.
#'
#' @param input An input value.
#' @param max_dim The maximum length of the input value.
#'
#' @return Either a single character string describing the first error encountered, or
#' NULL if no errors are found.
#' @export
chk_address_city <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_address_country <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_address_email <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 120)
}
#' @export
#' @rdname chk_address_city
chk_address_fax <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 60)
}
#' @export
#' @rdname chk_address_city
chk_address_lines <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_address_postal_code <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 10)
}
#' @export
#' @rdname chk_address_city
chk_address_state <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_address_web_page <- function(input, max_dim) {
  chk_input_size(input, max_dim, 4, 2047)
}
#' @export
#' @rdname chk_address_city
chk_address_country <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_adopted_by_which_parent <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_adoptive_parents())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_age_at_event <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 2, 13),
    chk_input_pattern(input, reg_age_at_event())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_attribute_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_attribute_type <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_attribute_types())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_automated_record_id <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 12)
}
#' @export
#' @rdname chk_address_city
chk_before_common_era <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, c("BCE", "BC", "B.C."))
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_caste_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_cause_of_event <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_certainty_assessment <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, as.character(0:3))
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_character_encoding <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, c("UTF-8", "UNICODE"))
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_copyright_gedcom_file <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_copyright_source_data <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_count_of_children <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 3)
}
#' @export
#' @rdname chk_address_city
chk_date_exact <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 10, 11),
    chk_input_pattern(input, reg_date_exact())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_date_period_covered <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 7, 35),
    chk_input_pattern(input, reg_date_period())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_date_value <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 1, 35),
    chk_input_pattern(input, reg_date_value())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_descriptive_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_event_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_event_or_fact_classification <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_event_type_cited_from <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 15)
}
#' @export
#' @rdname chk_address_city
chk_event_type_family <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_family_event_types())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_event_type_individual <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_individual_event_types())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_events_recorded <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_gedcom_content_description <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_gedcom_file_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 248)
}
#' @export
#' @rdname chk_address_city
chk_gedcom_form <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_pattern(input, "LINEAGE-LINKED")
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_gedcom_version_number <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3})?$")
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_id_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
#' @export
#' @rdname chk_address_city
chk_language_of_text <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 1, 15),
    chk_input_choice(input, val_languages())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_multimedia_file_reference <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 259)
}
#' @export
#' @rdname chk_address_city
chk_multimedia_format <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_multimedia_formats())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_name_of_business <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_name_of_product <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_name_of_repository <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_name_of_source_data <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_name_personal <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_name_phonetic <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_given <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_nickname <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_prefix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_suffix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_surname <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_name_piece_surname_prefix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
#' @export
#' @rdname chk_address_city
chk_name_romanised <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_name_type <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 30)
}
#' @export
#' @rdname chk_address_city
chk_national_or_tribal_origin <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_nobility_type_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_number_of_relationships <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 3)
}
#' @export
#' @rdname chk_address_city
chk_occupation <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_pedigree_linkage_type <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_pedigree_linkage_types())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_phone_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 25)
}
#' @export
#' @rdname chk_address_city
chk_phonetisation_method <- function(input, max_dim) {
  chk_input_size(input, max_dim, 4, 30)
}
#' @export
#' @rdname chk_address_city
chk_physical_description <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
#' @export
#' @rdname chk_address_city
chk_place_latitude <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 2, 10),
    chk_input_pattern(input, reg_latitude())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_place_longitude <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 2, 11),
    chk_input_pattern(input, reg_longitude())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_place_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_place_phonetic <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_place_romanised <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_possessions <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_product_version_number <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 3, 15),
    chk_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3}(\\.\\d{1,3})?)?$")
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_receiving_system_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
#' @export
#' @rdname chk_address_city
chk_relation_is_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 25)
}
#' @export
#' @rdname chk_address_city
chk_religious_affiliation <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
#' @export
#' @rdname chk_address_city
chk_responsible_agency <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_role_in_event <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 3, 27),
    chk_input_pattern(input, paste(reg_role_in_event(),
                                   reg_custom_value(), sep = "|"))
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_romanisation_method <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 30)
}
#' @export
#' @rdname chk_address_city
chk_scholastic_achievement <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_sex_value <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_sexes())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_source_call_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_source_descriptive_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
#' @export
#' @rdname chk_address_city
chk_source_filed_by_entry <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_source_jurisdiction_place <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
#' @export
#' @rdname chk_address_city
chk_source_media_type <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_choice(input, val_source_media_types())
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_source_originator <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 255)
}
#' @export
#' @rdname chk_address_city
chk_source_publication_facts <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
#' @export
#' @rdname chk_address_city
chk_submitter_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
#' @export
#' @rdname chk_address_city
chk_system_id <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
#' @export
#' @rdname chk_address_city
chk_text_from_source <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 32767)
}
#' @export
#' @rdname chk_address_city
chk_time_value <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim, 7, 12),
    chk_input_pattern(input, paste0("^\\d{1,2}:\\d\\d:\\d\\d$|",
                                    "^\\d{1,2}:\\d\\d:\\d\\d.\\d\\d$"))
  )[1]
}
#' @export
#' @rdname chk_address_city
chk_user_reference_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
#' @export
#' @rdname chk_address_city
chk_user_reference_type <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 40)
}
#' @export
#' @rdname chk_address_city
chk_user_text <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 32767)
}
#' @export
#' @rdname chk_address_city
chk_where_within_source <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
#' @export
#' @rdname chk_address_city
chk_xref <- function(input, max_dim) {
  c(
    chk_input_size(input, max_dim),
    chk_input_pattern(input, reg_xref(TRUE))
  )[1]
}

