# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value", "."))


.pkgenv <- new.env(parent=emptyenv())

.pkgenv$xref_prefix_indi <- "I"
.pkgenv$xref_prefix_famg <- "F"
.pkgenv$xref_prefix_subm <- "U"
.pkgenv$xref_prefix_repo <- "R"
.pkgenv$xref_prefix_obje <- "O"
.pkgenv$xref_prefix_note <- "N"
.pkgenv$xref_prefix_sour <- "S"
