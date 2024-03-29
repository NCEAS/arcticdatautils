context("Helpers")

mn <- env_load()$mn

test_that("a dummy package can be created", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  library(dataone)

  result <- create_dummy_package(mn)

  expect_true(object_exists(mn, result$metadata))
  expect_true(object_exists(mn, result$data))
  expect_true(object_exists(mn, result$resource_map))
})

test_that("create_dummy_package_full errors if wrong input", {
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  expect_error(create_dummy_package_full(mn, title = 11))
  expect_error(create_dummy_package_full("mn"))
})

test_that("A failed submission can be recovered", {
  #test runs without a token

  cn <- dataone::CNode('PROD')
  adc <- dataone::getMNode(cn, 'urn:node:ARCTIC')
  pids <- dataone::query(adc, list(q="fileName:(*eml_draft* AND -*Mullen*)",
                         fl = "id",
                         rows="50"))

  path <- tempfile(fileext = ".xml")

  recover_failed_submission(adc, pids[[1]]$id[1], path)

  doc <- EML::read_eml(path)
  expect_true(EML::eml_validate(doc))
})
