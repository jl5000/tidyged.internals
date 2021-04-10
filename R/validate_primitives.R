
#' @tests
#' expect_error(validate_input_size(1:2, 1))
#' expect_error(validate_input_size("123456", 1, min_char = 7))
#' expect_error(validate_input_size("123456", 1, max_char = 5))
validate_input_size <- function(input, max_dim, min_char = NULL, max_char = NULL) {
  if (length(input) > max_dim)
    stop("Input ", input, " has too many dimensions. The limit is ", max_dim)
  
  if (length(input) > 0 && !is.null(max_char) && max(nchar(input)) > max_char)
    stop("Input ", input, " has too many characters. The limit is ", max_char)
  
  if (length(input) > 0 && !is.null(min_char) && min(nchar(input)) < min_char)
    stop("Input ", input, " has too few characters. The minimum is ", min_char)
}

#' @tests
#' expect_error(validate_input_pattern("Test string", "Tast string"))
validate_input_pattern <- function(input, pattern) {
  if (length(input) > 0) {
    for (i in input) {
      if (!grepl(pattern, i))
        stop("Input ", i, " is in an unexpected format")
    }
  }
}

#' @tests
#' expect_error(validate_input_choice(20, 22:28))
validate_input_choice <- function(input, choices) {
  if (length(input) == 1 && !input %in% choices) 
    stop("Invalid argument value: ", input, ".\n  The valid values are: ", 
         paste(choices, collapse = ", "))
}


#' @tests
#' expect_error(validate_date(2005, day = 15))
#' expect_error(validate_date(month = 5))
#' expect_error(validate_date(2005, 13))
#' expect_error(validate_date(2005, 10, 32))
#' expect_error(validate_date(2005, -1, 6))
#' expect_error(validate_date(month = 1, day = 32))
validate_date <- function(year = numeric(),
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
#' expect_error(validate_dates("18 MAY 2005", "17 MAY 2005"))
#' expect_error(validate_dates("MAR 2005", "FEB 2004"))
#' expect_error(validate_dates("2005", "2004"))
validate_dates <- function(start_date, end_date) {

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

validate_address_city <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_country <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_email <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 120)
}
validate_address_fax <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 60)
}
validate_address_lines <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_postal_code <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 10)
}
validate_address_state <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_address_web_page <- function(input, max_dim) {
  validate_input_size(input, max_dim, 4, 2047)
}
validate_address_country <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_adopted_by_which_parent <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("HUSB", "WIFE", "BOTH")
  validate_input_choice(input, choices)
}
validate_age_at_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 2, 13)
  validate_input_pattern(input, age_at_event_pattern())
}
validate_attribute_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_attribute_type <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("CAST", "DSCR", "EDUC", "IDNO",
               "NATI", "NCHI", "NMR", "OCCU",
               "PROP", "RELI", "RESI",
               "TITL", "FACT")
  validate_input_choice(input, choices)
}
validate_automated_record_id <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 12)
}
validate_before_common_era <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("BCE", "BC", "B.C.")
  validate_input_choice(input, choices)
}
validate_caste_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_cause_of_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_certainty_assessment <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- as.character(0:3)
  validate_input_choice(input, choices)
}
validate_character_encoding <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("UTF-8", "UNICODE")
  validate_input_choice(input, choices)
}
validate_copyright_gedcom_file <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_copyright_source_data <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_count_of_children <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 3)
}
validate_date_exact <- function(input, max_dim) {
  validate_input_size(input, max_dim, 10, 11)
  validate_input_pattern(input, date_exact_pattern())
}
validate_date_period_covered <- function(input, max_dim) {
  validate_input_size(input, max_dim, 7, 35)
  validate_input_pattern(input, date_period_pattern())
}
validate_date_value <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 35)
  validate_input_pattern(input, date_value_pattern())
}
validate_descriptive_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_event_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_event_or_fact_classification <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_event_type_cited_from <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
}
validate_event_type_family <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("ANUL", "CENS", "DIV", "DIVF",
               "ENGA", "MARB", "MARC", "MARR",
               "MARL", "MARS", "RESI", "EVEN")
  validate_input_choice(input, choices)
}
validate_event_type_individual <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("BIRT", "CHR", "DEAT", "BURI", "CREM",
               "ADOP", "BAPM", "BARM", "BASM",
               "CHRA", "CONF", "FCOM", "NATU",
               "EMIG", "IMMI", "CENS", "PROB", "WILL",
               "GRAD", "RETI", "EVEN")
  validate_input_choice(input, choices)
}
validate_events_recorded <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_gedcom_content_description <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_gedcom_file_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 248)
}
validate_gedcom_form <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  validate_input_pattern(input, "LINEAGE-LINKED")
}
validate_gedcom_version_number <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  validate_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3})?$")
}

validate_id_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_language_of_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 15)
  choices <- languages()
  validate_input_choice(input, choices)
}
validate_multimedia_file_reference <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 259)
}
validate_multimedia_format <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- multimedia_formats()
  validate_input_choice(input, choices)
}
validate_name_of_business <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_product <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_repository <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_of_source_data <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_name_personal <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_phonetic <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece_given <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece_nickname <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_prefix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_suffix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_piece_surname <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_piece_surname_prefix <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 30)
}
validate_name_romanised <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_name_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 30)
}
validate_national_or_tribal_origin <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_nobility_type_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_number_of_relationships <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 3)
}
validate_occupation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_pedigree_linkage_type <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- pedigree_linkage_types()
  validate_input_choice(input, choices)
}
validate_phone_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_phonetisation_method <- function(input, max_dim) {
  validate_input_size(input, max_dim, 4, 30)
}
validate_physical_description <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 4095)
}
validate_place_latitude <- function(input, max_dim) {
  validate_input_size(input, max_dim, 2, 10)
  validate_input_pattern(input, latitude_pattern())
}
validate_place_longitude <- function(input, max_dim) {
  validate_input_size(input, max_dim, 2, 11)
  validate_input_pattern(input, longitude_pattern())
}
validate_place_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_phonetic <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_place_romanised <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_possessions <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_product_version_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 3, 15)
  validate_input_pattern(input, "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3}(\\.\\d{1,3})?)?$")
}
validate_receiving_system_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_relation_is_descriptor <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 25)
}
validate_religious_affiliation <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 90)
}
validate_responsible_agency <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_role_in_event <- function(input, max_dim) {
  validate_input_size(input, max_dim, 3, 27)
  validate_input_pattern(input, "CHIL|HUSB|WIFE|MOTH|FATH|SPOU|\\(.+\\)")
}
validate_romanisation_method <- function(input, max_dim) {
  validate_input_size(input, max_dim, 5, 30)
}
validate_scholastic_achievement <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_sex_value <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- c("M", "F", "U", "X", "N")
  validate_input_choice(input, choices)
}
validate_source_call_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_source_descriptive_title <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 4095)
}
validate_source_filed_by_entry <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_source_jurisdiction_place <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 120)
}
validate_source_media_type <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  choices <- source_media_types()
  validate_input_choice(input, choices)
}
validate_source_originator <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 255)
}
validate_source_publication_facts <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 4095)
}
validate_submitter_name <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 60)
}
validate_system_id <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_text_from_source <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 32767)
}
validate_time_value <- function(input, max_dim) {
  validate_input_size(input, max_dim, 7, 12)
  validate_input_pattern(input, paste0("^\\d{1,2}:\\d\\d:\\d\\d$|",
                                       "^\\d{1,2}:\\d\\d:\\d\\d.\\d\\d$"))
}
validate_user_reference_number <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 20)
}
validate_user_reference_type <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 40)
}
validate_user_text <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 32767)
}
validate_where_within_source <- function(input, max_dim) {
  validate_input_size(input, max_dim, 1, 248)
}
validate_xref <- function(input, max_dim) {
  validate_input_size(input, max_dim)
  validate_input_pattern(input, xref_pattern())
}

