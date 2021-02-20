

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



#' Identify the rows of a subrecord in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return A vector of rows in the tidyged object of the subrecord(s).
#' @export
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


#' Remove a subrecord in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tag The tag of the first line of the subrecord.
#' @param containing_value The value of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#'
#' @return The tidyged object with the subrecord(s) removed.
#' @export
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


#' Remove all creation dates from a tidyged object
#' 
#' @details This is a function used in tests so that the objects created do not
#' change every time.
#'
#' @param gedcom A tidyged object.
#'
#' @return The tidyged object with creation dates removed.
#' @export
remove_dates_for_tests <- function(gedcom) {
  
  gedcom %>% 
    remove_section(1, "CHAN", "") %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}

#' Make a dataframe a tidyged object
#' 
#' This function sets the attribute on a dataframe identifying it as a tidyged object.
#'
#' @param gedcom A tibble with content consistent with that of a tidyged object.
#'
#' @return A tidyged object.
#' @export
set_class_to_tidyged <- function(gedcom) {
  class(gedcom) <- c("tidyged", "tbl_df", "tbl", "data.frame")
  gedcom
}



#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param type An alphabetic sequence to be used as a prefix for the xref identifier.
#' GEDCOM files traditionally use a single letter to denote the type of record, e.g.
#' I for Individual, F for Family group, etc.
#' @param ref An explicit reference number after the type if one is to be chosen manually.
#' @param gedcom A tidyged object.
#'
#' @return An xref to use for a new record.
#' @export
assign_xref <- function(type = "", ref = 0, gedcom = tibble::tibble()) {
  
  if (ref == 0) {
    # Are there any existing records of this type?
    gedcom_filt <- gedcom %>% 
      dplyr::filter(stringr::str_detect(record, paste0("^@", type, "\\d+@$"))) 
    
    if(nrow(gedcom_filt) == 0) {
      ref <- 1
    } else {
      ref <- unique(gedcom_filt$record)
      
      ref <- ref[grepl(paste0("^@", type, "(\\d+)@$"), ref)] %>% 
        stringr::str_remove_all("@") %>% 
        stringr::str_remove_all("[A-Za-z]") %>% 
        as.numeric() %>% 
        max() + 1
      
    }
    
  } 
  paste0("@", type, ref, "@")
}

