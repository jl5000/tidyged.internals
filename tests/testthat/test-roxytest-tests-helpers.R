# Generated by roxytest: Do not edit by hand!

# File R/helpers.R: @tests

test_that("Function identify_section() @ L55", {
  expect_equal(identify_section(GEDCOM_HEADER(), 0, "HEAD", ""), 1:7)
  expect_equal(identify_section(GEDCOM_HEADER(), 1, "GEDC", ""), 2:5)
  expect_equal(identify_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), 4:5)
  expect_equal(identify_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), 5)
})


test_that("Function remove_section() @ L111", {
  expect_snapshot_value(remove_section(GEDCOM_HEADER(), 0, "HEAD", ""), "json2")
  expect_snapshot_value(remove_section(GEDCOM_HEADER(), 1, "GEDC", ""), "json2")
  expect_snapshot_value(remove_section(GEDCOM_HEADER(), 2, "FORM", "LINEAGE-LINKED"), "json2")
  expect_snapshot_value(remove_section(GEDCOM_HEADER(), 3, "VERS", "5.5.5"), "json2")
})


test_that("Function find_insertion_point() @ L244", {
  expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 2, "VERS"), 4)
  expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 3, "VERS"), 6)
  expect_equal(find_insertion_point(GEDCOM_HEADER(), "HD", 1, "CHAR"), 7)
})


test_that("Function gedcom_value() @ L282", {
  expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "FORM", 2), "LINEAGE-LINKED")
  expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "TEST", 1), "")
  expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "VERS", 2), "5.5.5")
  expect_equal(gedcom_value(GEDCOM_HEADER(), "HD", "VERS", 3), "5.5.5")
})


test_that("Function construct_full_name() @ L371", {
  expect_error(construct_full_name(surname_prefix = "de la"))
  expect_equal(construct_full_name(given = "Joe"), "Joe")
  expect_equal(construct_full_name(prefix = "Professor", given = "Joe"), "Professor Joe")
  expect_equal(construct_full_name(given = "Joe,Adam"), "Joe Adam")
  expect_equal(construct_full_name(given = "Joey,Joe, Joe"), "Joey Joe Joe")
  expect_equal(construct_full_name(surname = "Bloggs"), "/Bloggs/")
  expect_equal(construct_full_name(suffix = "Jr."), "Jr.")
  expect_equal(construct_full_name(suffix = "Jr.,Esq."), "Jr. Esq.")
  expect_equal(construct_full_name(given = "Joe,Adam",
                                   surname_prefix = "de la", surname = "Bloggs",
                                   suffix = "Jr., Esq."),
               "Joe Adam de la /Bloggs/ Jr. Esq.")
})

