

#' Get valid values for inputs
#' 
#' These functions return the valid values that are allowed by the GEDCOM Specification.
#'
#' @return A character vector of valid values.
#' @export
languages <- function() {
  c("Afrikaans", "Albanian", "Anglo-Saxon", "Catalan", "Catalan_Spn", "Czech", 
    "Danish", "Dutch", "English", "Esperanto", "Estonian", "Faroese", "Finnish", 
    "French", "German", "Hawaiian", "Hungarian", "Icelandic", "Indonesian", 
    "Italian", "Latvian", "Lithuanian", "Navaho", "Norwegian", "Polish", 
    "Portuguese", "Romanian", "Serbo_Croa", "Slovak", "Slovene", "Spanish", 
    "Swedish", "Turkish", "Wendic")
}

#' @rdname languages
#' @export
sexes <- function() {
  c("M", "F", "U", "X", "N")
}

#' @rdname languages
#' @export
multimedia_formats <- function() {
  c("AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "MKV",
    "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "WAV")
}

#' @rdname languages
#' @export
pedigree_linkage_types <- function() {
  c("adopted", "birth", "foster")
}

#' @rdname languages
#' @export
source_media_types <- function() {
  c("audio", "book", "card", "electronic", "fiche", 
    "film", "magazine", "manuscript", "map", 
    "newspaper", "photo", "tombstone", "video")
}

#' @rdname languages
#' @export
adoptive_parents <- function() {
  c(Husband = "HUSB", Wife = "WIFE", Both = "BOTH")
}

#' @rdname languages
#' @export
attribute_types <- function() {
  c(Caste = "CAST", 
    `Physical description` = "DSCR", 
    Education = "EDUC", 
    `National ID number` = "IDNO",
    Nationality = "NATI", 
    `Number of children` = "NCHI", 
    `Number of relationships` = "NMR", 
    Occupation = "OCCU",
    Possessions = "PROP", 
    Religion = "RELI", 
    Residence = "RESI",
    `Nobility title` = "TITL", 
    Other = "FACT")
}

#' @rdname languages
#' @export
family_event_types <- function() {
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
    Other = "EVEN")
}

#' @rdname languages
#' @export
individual_event_types <- function() {
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
    Other = "EVEN")
}