

#' Get valid values for inputs
#' 
#' These functions return the valid values that are allowed by the GEDCOM Specification.
#'
#' @return A character vector of valid values.
#' @export
val_languages <- function() {
  c("English", "Afrikaans", "Albanian", "Anglo-Saxon", "Catalan", "Catalan_Spn", "Czech", 
    "Danish", "Dutch", "Esperanto", "Estonian", "Faroese", "Finnish", 
    "French", "German", "Hawaiian", "Hungarian", "Icelandic", "Indonesian", 
    "Italian", "Latvian", "Lithuanian", "Navaho", "Norwegian", "Polish", 
    "Portuguese", "Romanian", "Serbo_Croa", "Slovak", "Slovene", "Spanish", 
    "Swedish", "Turkish", "Wendic")
}

#' @rdname val_languages
#' @export
val_sexes <- function() {
  c(Unknown = "U", Male = "M", Female = "F", Intersex = "X", `Not recorded` = "N")
}

#' @rdname val_languages
#' @export
val_multimedia_formats <- function() {
  c("AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "MKV",
    "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "WAV")
}

#' @rdname val_languages
#' @export
val_pedigree_linkage_types <- function() {
  c("birth", "adopted", "foster") %>% 
    setNames(stringr::str_to_title(.))
}

#' @rdname val_languages
#' @export
val_source_media_types <- function() {
  c("audio", "book", "card", "electronic", "fiche", 
    "film", "magazine", "manuscript", "map", 
    "newspaper", "photo", "tombstone", "video") %>% 
    setNames(stringr::str_to_title(.))
}

#' @rdname val_languages
#' @export
val_adoptive_parents <- function() {
  c(Husband = "HUSB", Wife = "WIFE", Both = "BOTH")
}

#' @rdname val_languages
#' @export
val_attribute_types <- function() {
  c(Caste = "CAST", 
    `Physical description` = "DSCR", 
    Education = "EDUC", 
    `National ID number` = "IDNO",
    Nationality = "NATI", 
    `Number of children` = "NCHI", 
    `Number of relationships` = "NMR", 
    Occupation = "OCCU",
    Property = "PROP", 
    Religion = "RELI", 
    Residence = "RESI",
    `Nobility title` = "TITL", 
    `Other attribute` = "FACT")
}

#' @rdname val_languages
#' @export
val_family_event_types <- function() {
  c(Annulment = "ANUL", 
    Census = "CENS", 
    Divorce = "DIV", 
    `Divorce filed` = "DIVF",
    Engagement = "ENGA", 
    `Marriage banns` = "MARB", 
    `Marriage contract` = "MARC", 
    Relationship = "MARR",
    `Marriage license` = "MARL", 
    `Marriage settlement` = "MARS", 
    Residence = "RESI", 
    `Other event` = "EVEN")
}

#' @rdname val_languages
#' @export
val_individual_event_types <- function() {
  c(Birth = "BIRT", 
    Christening = "CHR", 
    Death = "DEAT", 
    Burial = "BURI", 
    Cremation = "CREM",
    Adoption = "ADOP", 
    Baptism = "BAPM", 
    `Bar-mitzvah` = "BARM", 
    `Bas-mitzvah` = "BASM",
    `Adult christening` = "CHRA", 
    Confirmation = "CONF", 
    `First communion` = "FCOM", 
    Naturalization = "NATU",
    Emigration = "EMIG", 
    Immigration = "IMMI", 
    Census = "CENS", 
    Probate = "PROB", 
    Will = "WILL",
    Graduation = "GRAD", 
    Retirement = "RETI", 
    `Other event` = "EVEN")
}