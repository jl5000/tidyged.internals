# The structure of the functions in this file and the structures file all have a similar pattern
# First any inputs that could be numbers are converted to characters
# Then inputs are then checked to ensure they do not breach size limits
#   Structures are not checked for character length
# Then the outputs dataframe is constructed:
# Inputs which are not structures or lists are placed straight into tibbles
# Inputs which are lists of dataframes are first binded by row, and then levels pushed down
# Inputs which are structures have their levels pushed down
# Finally, if certain subordinate tags are not used, the parent tags are removed
# For records, an additional finalising step is performed which fills missing ids


#' Construct the GEDCOM_HEADER tibble
#' 
#' This function constructs a tibble representation of the GEDCOM_HEADER from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param header_extension A LINEAGE_LINKED_HEADER_EXTENSION() object providing more information
#' about the GEDCOM file.
#' @tests
#' expect_error(GEDCOM_HEADER("ANSEL"))
#' expect_snapshot_value(GEDCOM_HEADER(), "json2")
#' expect_snapshot_value(GEDCOM_HEADER(
#'         header_extension = LINEAGE_LINKED_HEADER_EXTENSION("tidyged.internals",
#'                                                            language_of_text = "English")), "json2")
#' 
#' @return A tidy tibble containing a GEDCOM_HEADER part of a GEDCOM file.
#' @export
GEDCOM_HEADER <- function(character_encoding = "UTF-8",
                          gedcom_version_number = "5.5.5",
                          gedcom_form = "LINEAGE-LINKED",
                          header_extension = LINEAGE_LINKED_HEADER_EXTENSION("tidyged.internals")) {
  
  gedcom_version_number <- as.character(gedcom_version_number)
  
  chk_character_encoding(character_encoding, 1) %>% parse_error()
  chk_gedcom_version_number(gedcom_version_number, 1) %>% parse_error()
  chk_gedcom_form(gedcom_form, 1) %>% parse_error()
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, record = "HD", tag = "HEAD", value = ""),
    tibble::tibble(level = 1, tag = "GEDC", value = ""),
    tibble::tibble(level = 2, tag = "VERS", value = gedcom_version_number),
    tibble::tibble(level = 2, tag = "FORM", value = gedcom_form),
    tibble::tibble(level = 3, tag = "VERS", value = gedcom_version_number),
    tibble::tibble(level = 1, tag = "CHAR", value = character_encoding),
    header_extension %>% add_levels(1),
  ) %>% 
    finalise()
  
}




#' Construct the FAMILY_GROUP_RECORD tibble
#' 
#' This function constructs a tibble representation of the FAMILY_GROUP_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param events A list of FAMILY_EVENT_STRUCTURE() objects giving events associated with this family.
#' @param xref_husb An xref ID of the husband.
#' @param xref_wife An xref ID of the wife.
#' @param xrefs_chil A vector of xref IDs of children in this family.
#' @tests
#' expect_error(FAMILY_GROUP_RECORD("@F1@", user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(FAMILY_GROUP_RECORD("@F1@"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@F1@", "FAM",                      "",
#'                              1, "@F1@", "CHAN",                     "",
#'                              2, "@F1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' expect_equal(FAMILY_GROUP_RECORD("@F1@", user_reference_number = c(123, 456),
#'                                  user_reference_type = c("type1", "type2")),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@F1@", "FAM",                      "",
#'                              1, "@F1@", "REFN",   "123",
#'                              2, "@F1@", "TYPE",   "type1",
#'                              1, "@F1@", "REFN",   "456",
#'                              2, "@F1@", "TYPE",   "type2",
#'                              1, "@F1@", "CHAN",                     "",
#'                              2, "@F1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a FAMILY_GROUP_RECORD part of a GEDCOM file.
#' @export
FAMILY_GROUP_RECORD <- function(xref_fam,
                                events = list(),
                                xref_husb = character(),
                                xref_wife = character(),
                                xrefs_chil = character(),
                                count_of_children = character(),
                                user_reference_number = character(),
                                user_reference_type = character(),
                                automated_record_id = character(),
                                date_changed = CHANGE_DATE(),
                                notes = list(),
                                source_citations = list(),
                                multimedia_links = list()){
  
  count_of_children <- as.character(count_of_children)
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_fam, 1) %>% parse_error()
  chk_xref(xref_husb, 1) %>% parse_error()
  chk_xref(xref_wife, 1) %>% parse_error()
  chk_xref(xrefs_chil, 100) %>% parse_error()
  chk_count_of_children(count_of_children, 1) %>% parse_error()
  chk_user_reference_number(user_reference_number, 1000) %>% parse_error()
  chk_user_reference_type(user_reference_type, 1000) %>% parse_error()
  chk_automated_record_id(automated_record_id, 1) %>% parse_error()
  
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, record = xref_fam, tag = "FAM", value = ""),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "HUSB", value = xref_husb),
    tibble::tibble(level = 1, tag = "WIFE", value = xref_wife),
    tibble::tibble(level = 1, tag = "CHIL", value = xrefs_chil),
    tibble::tibble(level = 1, tag = "NCHI", value = count_of_children)
  )
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


#' Construct the INDIVIDUAL_RECORD tibble
#' 
#' This function constructs a tibble representation of the INDIVIDUAL_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param names A list of PERSONAL_NAME_STRUCTURE() objects giving the names associated with this individual.
#' @param events A list of INDIVIDUAL_EVENT_STRUCTURE() objects giving the events associated with
#' this individual.
#' @param attributes A list of INDIVIDUAL_ATTRIBUTE_STRUCTURE() objects giving the attributes associated 
#' with this individual.
#' @param child_to_family_links A list of CHILD_TO_FAMILY_LINK() objects giving the details of families
#' this individual is a child of.
#' @param spouse_to_family_links A list of SPOUSE_TO_FAMILY_LINK() objects giving the details of families
#' this individual is a spouse of.
#' @param associations A list of ASSOCIATION_STRUCTURE() objects giving the details of individuals this
#' individual is associated with.
#' @tests
#' expect_error(INDIVIDUAL_RECORD("@I1@", user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(INDIVIDUAL_RECORD("@I1@"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@I1@", "INDI",                      "",
#'                              1, "@I1@", "CHAN",                      "",
#'                              2, "@I1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing an INDIVIDUAL_RECORD part of a GEDCOM file.
#' @export
INDIVIDUAL_RECORD <- function(xref_indi,
                              names = list(),
                              sex_value = character(),
                              events = list(),
                              attributes = list(),
                              child_to_family_links = list(),
                              spouse_to_family_links = list(),
                              associations = list(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE(),
                              notes = list(),
                              source_citations = list(),
                              multimedia_links = list()) {
  
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_indi, 1) %>% parse_error()
  chk_sex_value(sex_value, 1) %>% parse_error()
  chk_user_reference_number(user_reference_number, 1000) %>% parse_error()
  chk_user_reference_type(user_reference_type, 1000) %>% parse_error()
  chk_automated_record_id(automated_record_id, 1) %>% parse_error()
  
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, record = xref_indi, tag = "INDI", value = ""),
    names %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "SEX", value = sex_value),
    events %>% dplyr::bind_rows() %>% add_levels(1),
    attributes %>% dplyr::bind_rows() %>% add_levels(1),
    child_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    spouse_to_family_links %>% dplyr::bind_rows() %>% add_levels(1),
    associations %>% dplyr::bind_rows() %>%  add_levels(1)
  )
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  ) %>% 
    finalise()
  
}


#' Construct the MULTIMEDIA_RECORD tibble
#' 
#' This function constructs a tibble representation of the MULTIMEDIA_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @tests
#' expect_error(MULTIMEDIA_RECORD("@M1@", "file_ref", "JPEG",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(MULTIMEDIA_RECORD("@M1@", "file_ref", "JPEG"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@M1@", "OBJE",                      "",
#'                              1, "@M1@", "FILE",              "file_ref",
#'                              2, "@M1@", "FORM",                   "JPEG",
#'                              1, "@M1@", "CHAN",                      "",
#'                              2, "@M1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' expect_equal(MULTIMEDIA_RECORD("@M1@", "file_ref", "JPEG",
#'                                user_reference_number = 123, user_reference_type = "type"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@M1@", "OBJE",                      "",
#'                              1, "@M1@", "FILE",              "file_ref",
#'                              2, "@M1@", "FORM",                   "JPEG",
#'                              1, "@M1@", "REFN",                   "123",
#'                              2, "@M1@", "TYPE",                   "type",
#'                              1, "@M1@", "CHAN",                      "",
#'                              2, "@M1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a MULTIMEDIA_RECORD part of a GEDCOM file.
#' @export
MULTIMEDIA_RECORD <- function(xref_obje,
                              multimedia_file_reference,
                              multimedia_format,
                              source_media_type = character(),
                              descriptive_title = character(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              notes = list(),
                              source_citations = list(),
                              date_changed = CHANGE_DATE()){
  
  multimedia_file_reference <- as.character(multimedia_file_reference)
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_obje, 1)
  chk_multimedia_file_reference(multimedia_file_reference, 1)
  chk_multimedia_format(multimedia_format, 1)
  chk_source_media_type(source_media_type, 1)
  chk_descriptive_title(descriptive_title, 1)
  chk_user_reference_number(user_reference_number, 1000)
  chk_user_reference_type(user_reference_type, 1000)
  chk_automated_record_id(automated_record_id, 1)
  
  temp <- dplyr::bind_rows(
       tibble::tibble(level = 0, record = xref_obje, tag = "OBJE", value = ""),
       tibble::tibble(level = 1, tag = "FILE", value = multimedia_file_reference),
       tibble::tibble(level = 2, tag = "FORM", value = multimedia_format),
       tibble::tibble(level = 3, tag = "TYPE", value = source_media_type),
       tibble::tibble(level = 2, tag = "TITL", value = descriptive_title)
       )
    
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
    
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   notes %>% dplyr::bind_rows() %>% add_levels(1),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}


#' Construct the NOTE_RECORD tibble
#' 
#' This function constructs a tibble representation of the NOTE_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @tests
#' expect_error(NOTE_RECORD("@N1@", "This is a note",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(NOTE_RECORD("@N1@", "This is a note", 123, "type"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@N1@", "NOTE",        "This is a note",
#'                              1, "@N1@", "REFN",        "123",
#'                              2, "@N1@", "TYPE",        "type",
#'                              1, "@N1@", "CHAN",                      "",
#'                              2, "@N1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a NOTE_RECORD part of a GEDCOM file.
#' @export
NOTE_RECORD <- function(xref_note,
                        user_text,
                        user_reference_number = character(),
                        user_reference_type = character(),
                        automated_record_id = character(),
                        source_citations = list(),
                        date_changed = CHANGE_DATE()){
  
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_note, 1)
  chk_user_text(user_text, 1)
  chk_user_reference_number(user_reference_number, 1000)
  chk_user_reference_type(user_reference_type, 1000)
  chk_automated_record_id(automated_record_id, 1)
  
  temp <- tibble::tibble(level = 0, record = xref_note, tag = "NOTE", value = user_text)
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   source_citations %>% dplyr::bind_rows() %>% add_levels(1),
                   date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}


#' Construct the REPOSITORY_RECORD tibble
#' 
#' This function constructs a tibble representation of the REPOSITORY_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param address An ADDRESS_STRUCTURE() object giving details of the repository address.
#' @tests
#' expect_error(REPOSITORY_RECORD("@R1@", "Repo name",
#'                                user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(REPOSITORY_RECORD("@R1@", "Repo name",
#'                                user_reference_number = 123, user_reference_type = "type"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@R1@", "REPO",                      "",
#'                              1, "@R1@", "NAME",             "Repo name",
#'                              1, "@R1@", "REFN",             "123",
#'                              2, "@R1@", "TYPE",             "type",
#'                              1, "@R1@", "CHAN",                      "",
#'                              2, "@R1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a REPOSITORY_RECORD part of a GEDCOM file.
#' @export
REPOSITORY_RECORD <- function(xref_repo,
                              name_of_repository,
                              address = ADDRESS_STRUCTURE(),
                              notes = list(),
                              user_reference_number = character(),
                              user_reference_type = character(),
                              automated_record_id = character(),
                              date_changed = CHANGE_DATE()){

  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_repo, 1)
  chk_name_of_repository(name_of_repository, 1)
  chk_user_reference_number(user_reference_number, 1000)
  chk_user_reference_type(user_reference_type, 1000)
  chk_automated_record_id(automated_record_id, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, record = xref_repo, tag = "REPO", value = ""),
    tibble::tibble(level = 1, tag = "NAME", value = name_of_repository),
    address %>% add_levels(1),
    notes %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
    
  dplyr::bind_rows(temp,
                   tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                   date_changed %>% add_levels(1)
  ) %>% 
    finalise()
}


#' Construct the SOURCE_RECORD tibble
#' 
#' This function constructs a tibble representation of the SOURCE_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param date_period_covered A date_period() object associated with the period covered by the sourse. 
#' @param data_notes A list of NOTE_STRUCTURE() objects associated with the data in this source.
#' @tests
#' expect_error(SOURCE_RECORD("@S1@",
#'                            user_reference_number = 123:125, user_reference_type = letters[1:2]))
#' expect_equal(SOURCE_RECORD("@S1@", user_reference_number = 234, user_reference_type = "type"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@S1@", "SOUR",                      "",
#'                              1, "@S1@", "REFN",                      "234",
#'                              2, "@S1@", "TYPE",                      "type",
#'                              1, "@S1@", "CHAN",                      "",
#'                              2, "@S1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a SOURCE_RECORD part of a GEDCOM file.
#' @export
SOURCE_RECORD <- function(xref_sour,
                          events_recorded = character(),
                          date_period_covered = date_period(),
                          source_jurisdiction_place = character(),
                          responsible_agency = character(),
                          data_notes = list(),
                          source_originator = character(),
                          source_descriptive_title = character(),
                          source_filed_by_entry = character(),
                          source_publication_facts = character(),
                          text_from_source = character(),
                          source_repository_citations = list(),
                          user_reference_number = character(),
                          user_reference_type = character(),
                          automated_record_id = character(),
                          date_changed = CHANGE_DATE(),
                          notes = list(),
                          multimedia_links = list()){
  
  user_reference_number <- as.character(user_reference_number)
  
  if (length(user_reference_type) > 0 & length(user_reference_type) != length(user_reference_number))
    stop("The number of user reference types must be the same as the number of user reference numbers")
  
  chk_xref(xref_sour, 1)
  chk_events_recorded(events_recorded, 1000)
  chk_date_period_covered(date_period_covered, 1)
  chk_source_jurisdiction_place(source_jurisdiction_place, 1)
  chk_responsible_agency(responsible_agency, 1)
  chk_source_originator(source_originator, 1)
  chk_source_descriptive_title(source_descriptive_title, 1)
  chk_source_filed_by_entry(source_filed_by_entry, 1)
  chk_source_publication_facts(source_publication_facts, 1)
  chk_text_from_source(text_from_source, 1)
  chk_user_reference_number(user_reference_number, 1000)
  chk_user_reference_type(user_reference_type, 1000)
  chk_automated_record_id(automated_record_id, 1)
  
  temp <- dplyr::bind_rows(
    tibble::tibble(level = 0, record = xref_sour, tag = "SOUR", value = ""),
    tibble::tibble(level = 1, tag = "DATA", value = ""),
    tibble::tibble(level = 2, tag = "EVEN", value = events_recorded),
    tibble::tibble(level = 3, tag = "DATE", value = date_period_covered),
    tibble::tibble(level = 3, tag = "PLAC", value = source_jurisdiction_place),
    tibble::tibble(level = 2, tag = "AGNC", value = responsible_agency),
    data_notes %>% dplyr::bind_rows() %>% add_levels(2),
    tibble::tibble(level = 1, tag = "AUTH", value = source_originator),
    tibble::tibble(level = 1, tag = "TITL", value = source_descriptive_title),
    tibble::tibble(level = 1, tag = "ABBR", value = source_filed_by_entry),
    tibble::tibble(level = 1, tag = "PUBL", value = source_publication_facts),
    tibble::tibble(level = 1, tag = "TEXT", value = text_from_source),
    source_repository_citations %>% dplyr::bind_rows() %>% add_levels(1)
    )
  
  
  for (i in seq_along(user_reference_number)) {
    temp <- dplyr::bind_rows(temp,
                             tibble::tibble(level = 1, tag = "REFN", value = user_reference_number[i]))
    
    if (length(user_reference_type) > 0 )
      temp <- dplyr::bind_rows(temp,
                               tibble::tibble(level = 2, tag = "TYPE", value = user_reference_type[i])) 
  }
  
  temp <- dplyr::bind_rows(temp,
                           tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
                           date_changed %>% add_levels(1),
                           notes %>% dplyr::bind_rows() %>% add_levels(1),
                           multimedia_links %>% dplyr::bind_rows() %>% add_levels(1)
  )
  
  if (length(date_period_covered) + length(source_jurisdiction_place) == 0)
    temp <- dplyr::filter(temp, tag != "EVEN")
  
  if (length(events_recorded) + length(responsible_agency) + length(data_notes) == 0)
    temp <- dplyr::filter(temp, tag != "DATA")
  
  finalise(temp)
  
}




#' Construct the SUBMITTER_RECORD tibble
#' 
#' This function constructs a tibble representation of the SUBMITTER_RECORD from the GEDCOM 5.5.5
#' specification.
#'
#' @inheritParams primitive_definitions
#' @param address An ADDRESS_STRUCTURE() object giving address details of the submitter.
#' @tests
#' expect_equal(SUBMITTER_RECORD("@S1@", "Joe Bloggs"),
#'              tibble::tribble(~level,  ~record,   ~tag,                  ~value,
#'                              0, "@S1@", "SUBM",                      "",
#'                              1, "@S1@", "NAME",            "Joe Bloggs",
#'                              1, "@S1@", "CHAN",                      "",
#'                              2, "@S1@", "DATE", trimws(toupper(format(Sys.Date(), "%e %b %Y")))
#'              ))
#' @return A tidy tibble containing a SUBMITTER_RECORD part of a GEDCOM file.
#' @export
SUBMITTER_RECORD <- function(xref_subm,
                             submitter_name,
                             address = ADDRESS_STRUCTURE(),
                             multimedia_links = list(),
                             automated_record_id = character(),
                             notes = list(),
                             date_changed = CHANGE_DATE()){
  
  chk_xref(xref_subm, 1)
  chk_submitter_name(submitter_name, 1)
  chk_automated_record_id(automated_record_id, 1)
  
  dplyr::bind_rows(
    tibble::tibble(level = 0, record = xref_subm, tag = "SUBM", value = ""),
    tibble::tibble(level = 1, tag = "NAME", value = submitter_name),
    address %>% add_levels(1),
    multimedia_links %>% dplyr::bind_rows() %>% add_levels(1),
    tibble::tibble(level = 1, tag = "RIN", value = automated_record_id),
    notes %>% dplyr::bind_rows() %>% add_levels(1),
    date_changed %>% add_levels(1)
  ) %>% 
    finalise()
  
  
}

#' Construct the FOOTER_SECTION tibble
#' 
#' This function constructs a tibble representation of the FOOTER_SECTION from the GEDCOM 5.5.5
#' specification.
#'
#' @tests
#' expect_equal(FOOTER_SECTION(),
#'              tibble::tribble(~level,  ~record,   ~tag, ~value,
#'                              0, "TR", "TRLR",     ""
#'              ))
#' @return A tidy tibble containing a FOOTER_SECTION part of a GEDCOM file.
#' @export
FOOTER_SECTION <- function(){
  tibble::tibble(level = 0, record = "TR", tag = "TRLR", value = "") %>% 
    finalise()
}
