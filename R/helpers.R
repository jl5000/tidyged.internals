
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
#' @tests
#' expect_equal(identify_section(GEDCOM_HEADER(), 0, "HEAD", ""), 1:7)
#' expect_equal(identify_section(GEDCOM_HEADER(), 1, "GEDC", ""), 2:5)
#' expect_equal(identify_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), 4:5)
#' expect_equal(identify_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), 5)
identify_section <- function(gedcom,
                             containing_level,
                             containing_tag,
                             containing_value,
                             xrefs = character()) {
  
  no_xrefs_defined <- length(xrefs) == 0
  
  rows_to_return <- integer()
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active) {
      if(gedcom$level[i] <= containing_level) {
        # we've reached the end of the section
        break
      } else {
        rows_to_return <- c(rows_to_return, i)
      }
      
    }
    
    if(no_xrefs_defined || gedcom$record[i] %in% xrefs) {
      if(gedcom$level[i] == containing_level & gedcom$tag[i] == containing_tag &
         gedcom$value[i] == containing_value) {
        
        active <- TRUE
        rows_to_return <- c(rows_to_return, i) 
      } 
      
    }
  }
  rows_to_return
  
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
#' @tests
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 0, "HEAD", ""), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 1, "GEDC", ""), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), "json2")
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




assign_xref <- function(type = "", ref = 0, gedcom = tibble::tibble(), quantity = 1) {
  
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
    ref <- seq(ref, ref+quantity-1, 1)
  } 
  paste0("@", type, ref, "@")
}

#' Create a new xref for a record
#' 
#' This function is used to assign xrefs to new records that are created.
#'
#' @param gedcom A tidyged object.
#' @param ref An explicit reference number if one is to be chosen manually.
#' @param quantity The number of new xrefs to return.
#'
#' @return A vector of xrefs to use for a new record(s).
#' @export
assign_xref_indi <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_indi, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_famg <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_famg, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_sour <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_sour, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_repo <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_repo, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_media <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_obje, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_note <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_note, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_subm <- function(gedcom = tibble::tibble(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_subm, ref, gedcom, quantity)}


#' Find a particular row position in a tidyged object.
#' 
#' This is for inserting rows at the end of a record or subrecord.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record where the insertion point will be.
#' @param parent_level The level of the row where the insertion point will be.
#' @param parent_tag The tag of the row where the insertion point will be.
#' @param parent_value The value of the row where the insertion point will be.
#'
#' @return The row after the insertion point in the tidyged object.
#' @export
#' @tests
#' expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 2, "VERS"), 4)
#' expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 3, "VERS"), 6)
#' expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 1, "CHAR"), 7)
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
#' @export
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
      if(gedcom_filtered$tag[i] == tag & gedcom_filtered$level[i] == level)
        return(gedcom_filtered$value[i])
    }
    
    if(i == nrow(gedcom_filtered)) return("")
  }
  
}

#' Update particular values in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param record_xref The xref of the record in which to update the value.
#' @param tag The tag associated with the value.
#' @param level The level number of the value.
#' @param old_value The old value.
#' @param new_value The new value.
#' @param after_tag Whether the tag should be subordinate to this parent tag. 
#' @param after_value Whether the value should be subordinate to this parent value. 
#'
#' @return A tidyged object with the value updated.
#' @export
gedcom_value_update <- function(gedcom, record_xref, tag, level, old_value, new_value, 
                                after_tag = NULL, after_value = NULL) {
  
  if(nrow(gedcom) == 0) return(tibble::tibble())
  
  # Can we do a straightforward mutate?
  if(is.null(after_tag) & is.null(after_value)) {
    gedcom_filtered <- dplyr::filter(gedcom, record == record_xref, tag == tag, 
                                     level == level, value == old_value)
    
    if(nrow(gedcom_filtered) == 1)
      return(dplyr::mutate(gedcom, value = ifelse(record == record_xref & tag == tag & 
                                                    level == level & value == old_value, new_value, value)))
    
  }
  
  # We have to loop
  for(i in seq_len(nrow(gedcom))) {
    
    
    
  }
  
  
}

#' Construct a full personal name
#' 
#' This function constructs a full personal name from individual name pieces.
#'
#' @param given The given name(s).
#' @param surname_prefix The surname prefix.
#' @param surname The surname.
#' @param suffix The name suffix.
#'
#' @return The full name with all name pieces combined.
#' @export
#' @tests
#' expect_error(construct_full_name(surname_prefix = "de la"))
#' expect_equal(construct_full_name(given = "Joe"), "Joe")
#' expect_equal(construct_full_name(given = "Joe,Adam"), "Joe Adam")
#' expect_equal(construct_full_name(given = "Joey,Joe, Joe"), "Joey Joe Joe")
#' expect_equal(construct_full_name(surname = "Bloggs"), "/Bloggs/")
#' expect_equal(construct_full_name(suffix = "Jr."), "Jr.")
#' expect_equal(construct_full_name(suffix = "Jr.,Esq."), "Jr. Esq.")
#' expect_equal(construct_full_name(given = "Joe,Adam",
#'                                  surname_prefix = "de la", surname = "Bloggs",
#'                                  suffix = "Jr., Esq."),
#'              "Joe Adam de la /Bloggs/ Jr. Esq.")
construct_full_name <- function(given = character(), 
                                surname_prefix = character(), 
                                surname = character(), 
                                suffix = character()) {
  
  if(length(surname_prefix) == 1 & length(surname) == 0)
    stop("Surname prefix given without a surname")
  
  paste(
    stringr::str_replace_all(given, ", ?", " "), 
    surname_prefix, 
    ifelse(length(surname) == 1, paste0("/", surname, "/"), ""),
    stringr::str_replace_all(suffix, ", ?", " ")
  ) %>% 
    stringr::str_squish()
  
}