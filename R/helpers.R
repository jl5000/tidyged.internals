

#' Push a tidyged structure down a number of levels
#'
#' @param df A tidyged structure.
#' @param start_level How many levels to add on to the existing levels.
#'
#' @return The tidyged structure with modified levels.
#' @export
add_levels <- function(df, start_level) {
  
  if (nrow(df) == 0) return(df)
  
  df %>% 
    dplyr::mutate(level = start_level + level)
  
}



#' Finalise the formation of a tidyged record
#'
#' @param df A tidyged record tibble.
#' @param global_start_level A global start level for records (default is 0).
#'
#' @return A final tidyged record tibble.
#' @export
finalise <- function(df, global_start_level = 0) {
  
  df %>% 
    dplyr::mutate(level = global_start_level + level) %>%
    tidyr::fill(record)
  
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


