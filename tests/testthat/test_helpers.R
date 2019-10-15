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

test_that("list_submissions returns correct output", {
  cn <- dataone::CNode('PROD')
  adc <- dataone::getMNode(cn, 'urn:node:ARCTIC')
  if (!is_token_set(adc)) {
    skip("No token set. Skipping test.")
  }

  out <- list_submissions(adc, '2018-10-01', '2018-10-03')
  expect_equal(out$submitter_name[1], 'Rachel Obbard')
})

test_that("write a valid EML to the given path", {
  cn <- dataone::CNode('PROD')
  adc <- dataone::getMNode(cn, 'urn:node:ARCTIC')
  if (!is_token_set(adc)) {
    skip("No token set. Skipping test.")
  }

  create_dummy_package()

}

test_that("recover a failed submission", {
  cn <- dataone::CNode('PROD')
  adc <- dataone::getMNode(cn, 'urn:node:ARCTIC')
  if (!is_token_set(adc)) {
    skip("No token set. Skipping test.")
  }

  pids <- query(adc, list(q="fileName:(*eml_draft* AND -*Mullen*)",
                         fl = "id",
                         rows="50"))

  path <- tempfile(fileext = ".xml")

  recover_failed_submission(adc, pids[[1]]$id[1], path)

  doc <- EML::read_eml(path)
  expect_true(EML::eml_validate(doc))

  }
)
