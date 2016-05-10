#' test_name_substitution.R

context("name_substitution")

test_that("the contents of <surName> are expanded into their parts as we expect", {
  eml_file <- file.path(system.file("tests", "data", package = "arcticdata"), "name_sub_test-01.xml")
  tmp <- tempfile()
  expect_true(file.copy(eml_file, tmp))
  readLines(tmp)

  substitute_eml_party(tmp)

  doc <- xml2::read_xml(tmp)
  names <- lapply(xml2::as_list(xml2::xml_find_all(doc, "//individualName")), xml2::as_list)

  expect_true(length(names) == 5)
  expect_true(names[[1]]$givenName[[1]] == "Yuri")
  expect_true(names[[1]]$surName[[1]] == "Shur")
  expect_true(names[[2]]$givenName[[1]] == "Mikhail")
  expect_true(names[[2]]$surName[[1]] == "Kanevskiy")
  expect_true(names[[3]][[2]][[1]] == "Nancy")
  expect_true(names[[3]][[4]][[1]] == "H.")
  expect_true(names[[3]]$surName[[1]] == "Bigelow")
  expect_true(names[[4]]$givenName[[1]] == "James")
  expect_true(names[[4]]$surName[[1]] == "Beget")
  expect_true(names[[5]]$givenName[[1]] == "Yuri")
  expect_true(names[[5]]$surName[[1]] == "Shur")
})
