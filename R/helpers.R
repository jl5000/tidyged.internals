

`%nin%` <- Negate(`%in%`)


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


