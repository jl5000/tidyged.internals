
#' Push a tidyged structure down a number of levels
#'
#' @param df A tidyged structure.
#' @param start_level How many levels to add on to the existing levels.
#'
#' @return The tidyged structure with modified levels.
#' @export
add_levels <- function(df, start_level) {
  
  if (nrow(df) == 0) return(df)
  
  df |> 
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
  
  df |> 
    dplyr::mutate(level = global_start_level + level) |>
    tidyr::fill(record)
  
}



#' Identify the rows of subrecords in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tags The accepted tags of the first line of the subrecord.
#' @param containing_values The accepted values of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#' @param first_only Whether to return only the first match found or to return all matches. 
#'
#' @return A vector of rows in the tidyged object of the subrecord(s).
#' @export
#' @tests
#' expect_equal(identify_section(GEDCOM_HEADER(), 0, "HEAD", ""), 1:7)
#' expect_equal(identify_section(GEDCOM_HEADER(), 1, "GEDC", "", first_only = TRUE), 2:5)
#' expect_equal(identify_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), 4:5)
#' expect_equal(identify_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), 5)
identify_section <- function(gedcom,
                             containing_level,
                             containing_tags,
                             containing_values = character(),
                             xrefs = character(),
                             first_only = FALSE) {
  
  no_xrefs_defined <- length(xrefs) == 0
  no_values_defined <- length(containing_values) == 0
  rows_to_return <- integer()
  
  relevant_rows <- which(gedcom$level >= containing_level)
  if(!no_xrefs_defined) 
    relevant_rows <- setdiff(relevant_rows, which(!gedcom$record %in% xrefs))
  
  active <- FALSE
  for(i in relevant_rows) {
    
    if(active) {
      if(gedcom$level[i] <= containing_level) {
        active <- FALSE
        if(first_only) break
      } else {
        rows_to_return <- c(rows_to_return, i)
      }
      
    }
    
    if(no_xrefs_defined || gedcom$record[i] %in% xrefs) {
      if(no_values_defined || gedcom$value[i] %in% containing_values) {
        if(gedcom$level[i] == containing_level & gedcom$tag[i] %in% containing_tags) {
          
          active <- TRUE
          rows_to_return <- c(rows_to_return, i) 
        } 
      }
    }
  }
  rows_to_return
  
}


#' Remove subrecords in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tags The accepted tags of the first line of the subrecord.
#' @param containing_values The accepted values of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#' @param first_only Whether to remove only the first match found or to remove
#' all matches. 
#'
#' @return The tidyged object with the subrecord(s) removed.
#' @export
#' @tests
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 0, "HEAD", ""), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 1, "GEDC", ""), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), "json2")
#' expect_snapshot_value(remove_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), "json2")
#' expect_identical(remove_section(GEDCOM_HEADER(), 1, "GEDC1", ""), GEDCOM_HEADER())
remove_section <- function(gedcom,
                           containing_level,
                           containing_tags,
                           containing_values = character(),
                           xrefs = character(),
                           first_only = FALSE) {
  
  rows_to_remove <- identify_section(gedcom,
                                     containing_level,
                                     containing_tags,
                                     containing_values,
                                     xrefs,
                                     first_only)
  
  if(length(rows_to_remove) == 0) {
    gedcom
  } else {
    dplyr::slice(gedcom, -rows_to_remove)
  }
  
}




#' Make a dataframe a tidyged object
#' 
#' This function sets the attribute on a dataframe identifying it as a tidyged object.
#'
#' @param gedcom A tibble with content consistent with that of a tidyged object.
#'
#' @return A tidyged object.
#' @tests
#' expect_identical(class(set_class_to_tidyged(tibble::tibble())),
#'                  c("tidyged", "tbl_df", "tbl", "data.frame"))
#' @export
set_class_to_tidyged <- function(gedcom) {
  class(gedcom) <- c("tidyged", "tbl_df", "tbl", "data.frame")
  gedcom
}


empty_gedcom <- function(){
  tibble::tibble(level = integer(),
                 record = character(),
                 tag = character(),
                 value = character()) |>
    set_class_to_tidyged()
}


assign_xref <- function(type = "", ref = 0, gedcom = empty_gedcom(), quantity = 1) {
  
  if (ref == 0) {
    # Are there any existing records of this type?
    gedcom_filt <- gedcom[grepl(paste0("^@", type, "\\d+@$"), gedcom$record),]

    if(nrow(gedcom_filt) == 0) {
      ref <- 1:quantity
    } else {
      ref <- unique(gedcom_filt$record)
      
      existing_refs <- ref[grepl(paste0("^@", type, "\\d+@$"), ref)] |>
        str_remove_all("@") |> 
        str_remove_all("[A-Za-z]") |> 
        as.integer()
      
      available_refs <- seq_len(max(existing_refs) + quantity) |>
        dplyr::setdiff(existing_refs)
      
      ref <- available_refs[1:quantity]
    }
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
#' @tests
#' expect_equal(assign_xref_indi(tibble::tibble(record = "@I6@")), "@I1@")
#' expect_equal(assign_xref_famg(tibble::tibble(record = "@N6@")), "@F1@")
#' expect_equal(assign_xref_note(tibble::tibble(record = "@N2@"), quantity = 2), c("@N1@", "@N3@"))
#' expect_equal(assign_xref_repo(tibble::tibble(record = "@N6@")), "@R1@")
#' expect_equal(assign_xref_sour(tibble::tibble(record = "@S1@")), "@S2@")
#' expect_equal(assign_xref_media(tibble::tibble(record = "@S1@")), "@M1@")
#' expect_equal(assign_xref_subm(ref = 2), "@U2@")
#' @export
assign_xref_indi <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_indi, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_famg <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_famg, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_sour <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_sour, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_repo <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_repo, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_media <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_obje, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_note <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_note, ref, gedcom, quantity)}

#' @export
#' @rdname assign_xref_indi
assign_xref_subm <- function(gedcom = empty_gedcom(), ref = 0, quantity = 1) {assign_xref(.pkgenv$xref_prefix_subm, ref, gedcom, quantity)}

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
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "@I1@", "VERS", 3), "")
#' expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "VERS", 3, "FORM"), "5.5.5")
gedcom_value <- function(gedcom, record_xref, tag, level, after_tag = NULL) {
  
  relevant_rows <- which(gedcom$record %in% record_xref, gedcom$level >= level - 1)
  if(length(relevant_rows) == 0) return("")
  
  active <- is.null(after_tag)
  for(i in relevant_rows) {
    if(is.null(after_tag)) {
      active <- TRUE
    } else if(gedcom$tag[i] == after_tag && gedcom$level[i] < level) {
      active <- TRUE
    } else if(active && gedcom$level[i] < level){
      active <- FALSE
    }
    
    if(active) {
      if(gedcom$tag[i] == tag & gedcom$level[i] == level)
        return(gedcom$value[i])
    }
    
  }
  
  return("")
  
}


#' Construct a full personal name
#' 
#' This function constructs a full personal name from individual name pieces.
#'
#' @param prefix The name prefix.
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
#' expect_equal(construct_full_name(prefix = "Professor", given = "Joe"), "Professor Joe")
#' expect_equal(construct_full_name(given = "Joe,Adam"), "Joe Adam")
#' expect_equal(construct_full_name(given = "Joey,Joe, Joe"), "Joey Joe Joe")
#' expect_equal(construct_full_name(surname = "Bloggs"), "/Bloggs/")
#' expect_equal(construct_full_name(suffix = "Jr."), "Jr.")
#' expect_equal(construct_full_name(suffix = "Jr.,Esq."), "Jr. Esq.")
#' expect_equal(construct_full_name(given = "Joe,Adam",
#'                                  surname_prefix = "de la", surname = "Bloggs",
#'                                  suffix = "Jr., Esq."),
#'              "Joe Adam de la /Bloggs/ Jr. Esq.")
construct_full_name <- function(prefix = character(),
                                given = character(), 
                                surname_prefix = character(), 
                                surname = character(), 
                                suffix = character()) {
  
  if(length(surname_prefix) == 1 & length(surname) == 0)
    stop("Surname prefix given without a surname")
  
  if(length(surname) == 1) {
    surname <- paste0("/", surname, "/")
  } else {
    surname <- ""
  }
  
  fn <- paste(
    gsub(", ?", " ", prefix),
    gsub(", ?", " ", given), 
    surname_prefix, 
    surname,
    gsub(", ?", " ", suffix)
  )
  
  gsub("\\s+", " ", fn) |>
    trimws()
  
}

# Stringr dependency removed
str_extract <- function(string, pattern){
  regmatches(string, regexpr(pattern, string))
}
str_extract_all <- function(string, pattern){
  regmatches(string, gregexpr(pattern, string))
}
str_replace <- function(string, pattern, replacement){
  sub(pattern, replacement, string)
}
str_replace_all <- function(string, pattern, replacement){
  gsub(pattern, replacement, string)
}
str_remove <- function(string, pattern){
  sub(pattern, "", string)
}
str_remove_all <- function(string, pattern){
  gsub(pattern, "", string)
}