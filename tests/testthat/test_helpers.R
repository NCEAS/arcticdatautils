#' test_helpers.R

context("helpers")

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


test_that("all system metadata is retrieved", {
  expect_error(get_all_sysmeta(node, rm_pid))

  cn_staging <- CNode("STAGING")
  adc_test <- getMNode(cn_staging, "urn:node:mnTestARCTIC")

  expect_error(get_all_sysmeta(adc_test, test))
  expect_error(get_all_sysmeta(adc_test, ""))
  expect_error(get_all_sysmeta(adc_test, "urn:uuid:3e3bb5de-ec63-4f13-a549-813f0cf28610"))
  expect_error(get_all_sysmeta(adc_test, rm_pid, nmax = -7))
  expect_error(get_all_sysmeta(adc_test, rm_pid, child_packages = 7))


  rm_pid <- "resource_map_urn:uuid:3e3bb5de-ec63-4f13-a549-813f0cf28610"
  all <- get_all_sysmeta(adc_test, rm_pid)

  expect_message(get_all_sysmeta(adc_test, rm_pid))
  expect_type(all, "list")
  expect_length(all, 5)
  expect_equal(names(all)[1], "dummy_resource_map.xml")

  expect_message(get_all_sysmeta(adc_test, "resource_map_urn:uuid:924f81f6-2e68-4eb8-925f-53f5b66318ec"))
})
