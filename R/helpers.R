

`%nin%` <- Negate(`%in%`)



xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

xrefs_individuals <-  function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }
xrefs_families <-     function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_fam)  }
xrefs_submitters <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }
xrefs_sources <-      function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }
xrefs_repositories <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }
xrefs_notes <-        function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }
xrefs_multimedia <-   function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }



#' Push a tidygedcom structure down a number of levels
#'
#' @param df A tidygedcom structure.
#' @param start_level How many levels to add on to the existing levels.
#'
#' @return The tidygedcom structure with modified levels.
add_levels <- function(df, start_level) {
  
  if (nrow(df) == 0) return(df)
  
  df %>% 
    dplyr::mutate(level = start_level + level)
  
}



#' Finalise the formation of a tidygedcom record
#'
#' @param df A tidygedcom record tibble.
#' @param global_start_level A global start level for records (default is 0).
#'
#' @return A final tidygedcom record tibble.
finalise <- function(df, global_start_level = 0) {
  
  df %>% 
    dplyr::mutate(level = global_start_level + level) %>%
    tidyr::fill(record)
  
}




#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param type The type of record, given by one of the xref_prefix_*() functions.
#' @param ref An explicit reference string (xref without the "@") if one is to be chosen manually.
#' @param gedcom A tidygedcom object
#'
#' @return An xref to use for a new record.
assign_xref <- function(type, ref = 0, gedcom = tibble::tibble()) {
  
  if (ref == 0) {
    gedcom_filt <- gedcom %>% 
      dplyr::filter(stringr::str_detect(record, paste0("@", type, "\\d+@"))) 
    
    if(nrow(gedcom_filt) == 0) {
      ref <- 1
    } else {
      ref <- gedcom_filt %>%
        dplyr::pull(record) %>% 
        unique() %>% 
        stringr::str_remove_all("@") %>% 
        stringr::str_remove_all("[A-Z]") %>% 
        as.numeric() %>% 
        max() + 1
      
    }
    
  }
  paste0("@", type, ref, "@")
  
}



#' Salvage a surname from name pieces
#'
#' @param full_name The full name of the individual.
#' @param name_pieces The PERSONAL_NAME_PIECES() object associated with the name.
#'
#' @return A better populated PERSONAL_NAME_PIECES() object.
salvage_name_pieces <- function(full_name, name_pieces) {
  
  if(nrow(name_pieces) > 0) return(name_pieces)
  
  if(stringr::str_detect(full_name, "/.+/")) {
    
    surname <- full_name %>% 
      stringr::str_extract("/.+/") %>% 
      stringr::str_remove_all("/")
    
    name_pieces <- PERSONAL_NAME_PIECES(name_piece_surname = surname)
    
  } else {
    stop("The name ", full_name, " is given without any name pieces")
  }
  
  name_pieces
}



#' Identify the rows of a subrecord in a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return A vector of rows in the tidygedcom object of the subrecord(s).
identify_section <- function(gedcom,
                           containing_level,
                           containing_tag,
                           containing_value,
                           xrefs = character()) {
  
  no_xrefs_defined <- length(xrefs) == 0
  
  rows_to_remove <- integer()
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active) {
      if(gedcom$level[i] <= containing_level) {
        active <- FALSE
      } else {
        rows_to_remove <- c(rows_to_remove, i)
      }
      
    }
    
    if(no_xrefs_defined || gedcom$record[i] %in% xrefs) {
      if(gedcom$level[i] == containing_level & gedcom$tag[i] == containing_tag &
         gedcom$value[i] == containing_value) {
        
        active <- TRUE
        rows_to_remove <- c(rows_to_remove, i) 
      } 
      
    }
  }
  rows_to_remove
  
}


#' Remove a subrecord in a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return The tidygedcom object with the subrecord(s) removed.
remove_section <- function(gedcom,
                           containing_level,
                           containing_tag,
                           containing_value,
                           xrefs = character()) {
  
  rows_to_remove <- identify_section(gedcom,
                                     containing_level,
                                     containing_tag,
                                     containing_value,
                                     xrefs)
  
  if(length(rows_to_remove) == 0) {
    gedcom
  } else {
    dplyr::slice(gedcom, -rows_to_remove)
  }
  
}


#' Remove all creation dates from a tidygedcom object
#' 
#' @details This is a function used in tests so that the objects created do not
#' change every time.
#'
#' @param gedcom A tidygedcom object.
#'
#' @return The tidygedcom object with creation dates removed.
remove_dates_for_tests <- function(gedcom) {
  
  gedcom %>% 
    remove_change_dates() %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}


#' Remove all context lines from test files
#' 
#' @details This is a function used in development run after devtools::document().
#' It removes all context() lines from test files inserted automatically by roxytest.
#'
#' @return Nothing.
remove_context_from_tests <- function() {
  
  files <- list.files("tests/testthat", "^test-.+\\.R$", full.names = TRUE)
  
  for (file in files) {
    text <- readLines(file)
    text <- purrr::discard(text, ~ substr(., 1, 7) == "context")
    writeLines(text, file)
  }
  
}

#' Remove all CHANge dates from a tidygedcom object
#'
#' @param gedcom A tidygedcom object.
#'
#' @return A tidygedcom object with all CHAN structures removed.
#' @export
remove_change_dates <- function(gedcom) {
  
  gedcom %>% 
    remove_section(1, "CHAN", "")
  
}
