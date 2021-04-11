
#' @tests
#' expect_error(chk_input_size(1:2, 1))
#' expect_error(chk_input_size("123456", 1, min_char = 7))
#' expect_error(chk_input_size("123456", 1, max_char = 5))
chk_input_size <- function(input, max_dim, min_char = NULL, max_char = NULL) {
  if (length(input) > max_dim)
    stop("Input ", input, " has too many dimensions. The limit is ", max_dim)
  
  if (length(input) > 0 && !is.null(max_char) && max(nchar(input)) > max_char)
    stop("Input ", input, " has too many characters. The limit is ", max_char)
  
  if (length(input) > 0 && !is.null(min_char) && min(nchar(input)) < min_char)
    stop("Input ", input, " has too few characters. The minimum is ", min_char)
}

#' @tests
#' expect_error(chk_input_pattern("Test string", "Tast string"))
chk_input_pattern <- function(input, pattern) {
  if (length(input) > 0) {
    for (i in input) {
      if (!grepl(pattern, i))
        stop("Input ", i, " is in an unexpected format")
    }
  }
}

#' @tests
#' expect_error(chk_input_choice(20, 22:28))
chk_input_choice <- function(input, choices) {
  if (length(input) == 1 && !input %in% choices) 
    stop("Invalid argument value: ", input, ".\n  The valid values are: ", 
         paste(choices, collapse = ", "))
}


#' @tests
#' expect_error(chk_date(2005, day = 15))
#' expect_error(chk_date(month = 5))
#' expect_error(chk_date(2005, 13))
#' expect_error(chk_date(2005, 10, 32))
#' expect_error(chk_date(2005, -1, 6))
#' expect_error(chk_date(month = 1, day = 32))
chk_date <- function(year = numeric(),
                          month = numeric(),
                          day = numeric()) {
  
  if (length(year) + length(day) < length(month))
    stop("Month is defined without a day or year")
  
  if (length(month) < length(day))
    stop("Day is defined without a month")
  
  # Set empties to something reasonable
  if (length(year) == 0) year <- 2000
  if (length(month) == 0) month <- 1
  if (length(day) == 0) day <- 1
  
  #Let lubridate do the heavy lifting
  test_date <- lubridate::make_date(year, month, day)
  if (is.na(test_date)) stop("Date is invalid")
}


#' @tests
#' expect_error(chk_dates("18 MAY 2005", "17 MAY 2005"))
#' expect_error(chk_dates("MAR 2005", "FEB 2004"))
#' expect_error(chk_dates("2005", "2004"))
chk_dates <- function(start_date, end_date) {

  start_date <- start_date %>% 
    stringr::str_remove("/\\d{2}") 
  
  end_date <- end_date %>% 
    stringr::str_remove("/\\d{2}") 
  
  # Set first date to earliest possible time
  if(stringr::str_detect(start_date, "\\d{3,4}$")) {
    start_year <- stringr::str_extract(start_date, "\\d{3,4}$")
  } else {
    start_year <- 1000
  }
  
  if(stringr::str_detect(start_date, "[A-Z]{3}")) {
    start_month <- which(toupper(month.abb) == stringr::str_extract(start_date, "[A-Z]{3}"))
  } else {
    start_month <- 1
  }
  
  if(stringr::str_detect(start_date, "^\\d{1,2} ")) {
    start_day <- stringr::str_extract(start_date, "^\\d{1,2} ") %>% stringr::str_trim()
  } else {
    start_day <- 1
  }
  # Set end date to latest possible time
  if(stringr::str_detect(end_date, "\\d{3,4}$")) {
    end_year <- stringr::str_extract(end_date, "\\d{3,4}$")
  } else {
    end_year <- 4000
  }
  
  if(stringr::str_detect(end_date, "[A-Z]{3}")) {
    end_month <- which(toupper(month.abb) == stringr::str_extract(end_date, "[A-Z]{3}"))
  } else {
    end_month <- 12
  }
  
  if(stringr::str_detect(end_date, "^\\d{1,2} ")) {
    end_day <- stringr::str_extract(end_date, "^\\d{1,2} ") %>% stringr::str_trim()
  } else {
    end_day <- lubridate::days_in_month(lubridate::make_date(end_year, end_month))
  }

  date1 <- lubridate::make_date(start_year, start_month, start_day)
  date2 <- lubridate::make_date(end_year, end_month, end_day)

  if (date1 > date2) stop("First date is after second date")
  
}

chk_address_city <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_address_country <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_address_email <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 120)
}
chk_address_fax <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 60)
}
chk_address_lines <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_address_postal_code <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 10)
}
chk_address_state <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_address_web_page <- function(input, max_dim) {
  chk_input_size(input, max_dim, 4, 2047)
}
chk_address_country <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_adopted_by_which_parent <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_adoptive_parents()
  chk_input_choice(input, choices)
}
chk_age_at_event <- function(input, max_dim) {
  chk_input_size(input, max_dim, 2, 13)
  chk_input_pattern(input, reg_age_at_event())
}
chk_attribute_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_attribute_type <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_attribute_types()
  chk_input_choice(input, choices)
}
chk_automated_record_id <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 12)
}
chk_before_common_era <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- c("BCE", "BC", "B.C.")
  chk_input_choice(input, choices)
}
chk_caste_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_cause_of_event <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_certainty_assessment <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- as.character(0:3)
  chk_input_choice(input, choices)
}
chk_character_encoding <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- c("UTF-8", "UNICODE")
  chk_input_choice(input, choices)
}
chk_copyright_gedcom_file <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_copyright_source_data <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_count_of_children <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 3)
}
chk_date_exact <- function(input, max_dim) {
  chk_input_size(input, max_dim, 10, 11)
  chk_input_pattern(input, reg_date_exact())
}
chk_date_period_covered <- function(input, max_dim) {
  chk_input_size(input, max_dim, 7, 35)
  chk_input_pattern(input, reg_date_period())
}
chk_date_value <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 35)
  chk_input_pattern(input, reg_date_value())
}
chk_descriptive_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_event_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_event_or_fact_classification <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_event_type_cited_from <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 15)
}
chk_event_type_family <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_family_event_types()
  chk_input_choice(input, choices)
}
chk_event_type_individual <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_individual_event_types()
  chk_input_choice(input, choices)
}
chk_events_recorded <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_gedcom_content_description <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_gedcom_file_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 248)
}
chk_gedcom_form <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  chk_input_pattern(input, "LINEAGE-LINKED")
}
chk_gedcom_version_number <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  chk_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3})?$")
}

chk_id_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
chk_language_of_text <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 15)
  choices <- val_languages()
  chk_input_choice(input, choices)
}
chk_multimedia_file_reference <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 259)
}
chk_multimedia_format <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_multimedia_formats()
  chk_input_choice(input, choices)
}
chk_name_of_business <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_name_of_product <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_name_of_repository <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_name_of_source_data <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_name_personal <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_name_phonetic <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_name_piece_given <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_name_piece_nickname <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
chk_name_piece_prefix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
chk_name_piece_suffix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
chk_name_piece_surname <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_name_piece_surname_prefix <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 30)
}
chk_name_romanised <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_name_type <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 30)
}
chk_national_or_tribal_origin <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_nobility_type_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_number_of_relationships <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 3)
}
chk_occupation <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_pedigree_linkage_type <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_pedigree_linkage_types()
  chk_input_choice(input, choices)
}
chk_phone_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 25)
}
chk_phonetisation_method <- function(input, max_dim) {
  chk_input_size(input, max_dim, 4, 30)
}
chk_physical_description <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
chk_place_latitude <- function(input, max_dim) {
  chk_input_size(input, max_dim, 2, 10)
  chk_input_pattern(input, reg_latitude())
}
chk_place_longitude <- function(input, max_dim) {
  chk_input_size(input, max_dim, 2, 11)
  chk_input_pattern(input, reg_longitude())
}
chk_place_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_place_phonetic <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_place_romanised <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_possessions <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_product_version_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 3, 15)
  chk_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3}(\\.\\d{1,3})?)?$")
}
chk_receiving_system_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
chk_relation_is_descriptor <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 25)
}
chk_religious_affiliation <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 90)
}
chk_responsible_agency <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_role_in_event <- function(input, max_dim) {
  chk_input_size(input, max_dim, 3, 27)
  chk_input_pattern(input, "CHIL|HUSB|WIFE|MOTH|FATH|SPOU|\\(.+\\)")
}
chk_romanisation_method <- function(input, max_dim) {
  chk_input_size(input, max_dim, 5, 30)
}
chk_scholastic_achievement <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_sex_value <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_sexes()
  chk_input_choice(input, choices)
}
chk_source_call_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_source_descriptive_title <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
chk_source_filed_by_entry <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_source_jurisdiction_place <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 120)
}
chk_source_media_type <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  choices <- val_source_media_types()
  chk_input_choice(input, choices)
}
chk_source_originator <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 255)
}
chk_source_publication_facts <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 4095)
}
chk_submitter_name <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 60)
}
chk_system_id <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
chk_text_from_source <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 32767)
}
chk_time_value <- function(input, max_dim) {
  chk_input_size(input, max_dim, 7, 12)
  chk_input_pattern(input, paste0("^\\d{1,2}:\\d\\d:\\d\\d$|",
                                       "^\\d{1,2}:\\d\\d:\\d\\d.\\d\\d$"))
}
chk_user_reference_number <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 20)
}
chk_user_reference_type <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 40)
}
chk_user_text <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 32767)
}
chk_where_within_source <- function(input, max_dim) {
  chk_input_size(input, max_dim, 1, 248)
}
chk_xref <- function(input, max_dim) {
  chk_input_size(input, max_dim)
  chk_input_pattern(input, reg_xref())
}

