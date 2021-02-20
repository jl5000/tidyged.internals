
`%nin%` <- Negate(`%in%`)

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


#' Find a particular row position in a tidyged object.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record where the insertion point will be.
#' @param parent_level The level of the row where the insertion point will be.
#' @param parent_tag The tag of the row where the insertion point will be.
#' @param parent_value The value of the row where the insertion point will be.
#'
#' @return The row after the insertion point in the tidyged object.
find_insertion_point <- function(gedcom,
                                 xref,
                                 parent_level,
                                 parent_tag,
                                 parent_value = NULL) {
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active && gedcom$level[i] <= parent_level) break
    
    if(gedcom$record[i] == xref && gedcom$level[i] == parent_level && gedcom$tag[i] == parent_tag) {
      if(is.null(parent_value) || gedcom$value[i] == parent_value) {
        active <- TRUE  
      }
    } 
    
  }
  i
}


#' Extract a particular value from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param record_xref The xref of the record in which the value may exist.
#' @param tag The tag associated with the value.
#' @param level The level number of the value.
#' @param after_tag Whether the tag should be subordinate to this parent tag. 
#'
#' @return The particular value fitting the criteria of the input arguments. If no value is found,
#' an empty string is returned.
#' @tests
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "FORM", 2), "LINEAGE-LINKED")
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "TEST", 1), "")
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "VERS", 2), "5.5.5")
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "VERS", 3), "5.5.5")
gedcom_value <- function(gedcom, record_xref, tag, level, after_tag = NULL) {
  
  gedcom_filtered <- dplyr::filter(gedcom, record %in% record_xref)
  if(nrow(gedcom_filtered) == 0) return("")
  
  active <- is.null(after_tag)
  for(i in seq_len(nrow(gedcom_filtered))) {
    if(is.null(after_tag)) {
      active <- TRUE
    } else if(gedcom_filtered$tag[i] == after_tag && gedcom_filtered$level[i] < level) {
      active <- TRUE
    } else if(active && gedcom_filtered$level[i] < level){
      active <- FALSE
    }
    
    if(active) {
      if(gedcom_filtered$tag[i] == tag & gedcom_filtered$level[i] == level) break  
    }
    
    if(i == nrow(gedcom_filtered)) return("")
  }
  
  if(i == nrow(gedcom_filtered)) return(gedcom_filtered$value[i])
  
  for(j in (i+1):nrow(gedcom_filtered)) {
    if(gedcom_filtered$tag[j] %nin% c("CONT", "CONC") | 
       gedcom_filtered$level[j] != level + 1) {
      j <- j - 1
      break
    }
  }
  
  if(i == j) return(gedcom_filtered$value[i])
  
  text <- gedcom_filtered$value[i]
  for(row in (i+1):j) {
    if(gedcom_filtered$tag[row] == "CONT") text <- paste0(text, "\n", gedcom_filtered$value[row])
    if(gedcom_filtered$tag[row] == "CONC") text <- paste0(text, gedcom_filtered$value[row])
  }
  
  cat(text)
}

