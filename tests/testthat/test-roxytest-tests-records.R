# Generated by roxytest: Do not edit by hand!

# File R/records.R: @tests

test_that("Function GEDCOM_HEADER() @ L30", {
  expect_error(GEDCOM_HEADER("ANSEL"))
  expect_snapshot_value(GEDCOM_HEADER(), "json2")
  expect_snapshot_value(GEDCOM_HEADER(
          header_extension = LINEAGE_LINKED_HEADER_EXTENSION("tidyged.internals",
                                                             language_of_text = "English")), "json2")
})


test_that("Function FAMILY_GROUP_RECORD() @ L74", {
  expect_equal(FAMILY_GROUP_RECORD("@F1@") |> remove_section(1, "CHAN"),
               tibble::tibble(level = 0, record = "@F1@", tag = "FAM", value = ""))
  expect_snapshot_value(FAMILY_GROUP_RECORD("@F1@", user_reference_number = c(type1 = 123, 456)) |> 
                                                      remove_section(1, "CHAN"), "json2")
})


test_that("Function INDIVIDUAL_RECORD() @ L160", {
  expect_snapshot_value(INDIVIDUAL_RECORD("@I1@", sex_value = "X", user_reference_number = c(type = "123")) |> 
                                   remove_section(1, "CHAN"), "json2")
  expect_snapshot_value(INDIVIDUAL_RECORD("@I1@", sex_value = "X", user_reference_number = 234) |> 
                                   remove_section(1, "CHAN"), "json2")
})


test_that("Function MULTIMEDIA_RECORD() @ L234", {
  expect_snapshot_value(MULTIMEDIA_RECORD("@M1@", "file_ref", "JPEG") |> remove_section(1, "CHAN"), "json2")
  expect_snapshot_value(MULTIMEDIA_RECORD("@M1@", "file_ref", "JPEG", "electronic",
                                 user_reference_number = c(type = 123)) |> remove_section(1, "CHAN"), "json2")
})


test_that("Function NOTE_RECORD() @ L305", {
  expect_snapshot_value(NOTE_RECORD("@N1@", "This is a note", c(type = 123)) |> remove_section(1, "CHAN"),
                                                                    "json2")
  expect_snapshot_value(NOTE_RECORD("@N1@", "This is a note", 123) |> remove_section(1, "CHAN"),
                                                                    "json2")
})


test_that("Function REPOSITORY_RECORD() @ L364", {
  expect_snapshot_value(REPOSITORY_RECORD("@R1@", "Repo name",
                                 user_reference_number = c(type = 123)) |> remove_section(1, "CHAN"),
                                 "json2")
  expect_snapshot_value(REPOSITORY_RECORD("@R1@", "Repo name",
                                 user_reference_number = 123) |> remove_section(1, "CHAN"),
                                 "json2")
})


test_that("Function SOURCE_RECORD() @ L425", {
  expect_snapshot_value(SOURCE_RECORD("@S1@", user_reference_number = c(type = 234)) |> 
                           remove_section(1, "CHAN"), "json2")
  expect_snapshot_value(SOURCE_RECORD("@S1@", user_reference_number = 234) |> 
                           remove_section(1, "CHAN"), "json2")
})


test_that("Function SUBMITTER_RECORD() @ L522", {
  expect_snapshot_value(SUBMITTER_RECORD("@S1@", "Joe Bloggs") |> remove_section(1, "CHAN"), "json2")
})


test_that("Function FOOTER_SECTION() @ L558", {
  expect_equal(FOOTER_SECTION(),
               tibble::tibble(level = 0, record = "TR", tag = "TRLR", value = ""))
})

