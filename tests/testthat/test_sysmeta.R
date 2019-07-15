context("System metadata")

test_that("the replication policy gets cleared", {
  library(datapack)

  sysmeta <- new("SystemMetadata")
  expect_true(sysmeta@replicationAllowed)

  sysmeta <- clear_replication_policy(sysmeta)
  expect_false(sysmeta@replicationAllowed)
})

test_that("the replication policy gets defaulted correctly", {
  # this is just a regression test

  library(datapack)

  sysmeta <- new("SystemMetadata")
  sysmeta <- clear_replication_policy(sysmeta)

  expect_false(sysmeta@replicationAllowed)
  expect_equal(sysmeta@numberReplicas, 0)
  expect_identical(sysmeta@blockedNodes, list("urn:node:KNB", "urn:node:mnUCSB1"))
})

test_that("all system metadata is retrieved", {
  cn_staging <- tryCatch(CNode("STAGING"), error = function(e) CNode("STAGING"))
  adc_test <- getMNode(cn_staging, "urn:node:mnTestARCTIC")

  rm_pid <- "resource_map_urn:uuid:3e3bb5de-ec63-4f13-a549-813f0cf28610"

  expect_error(get_all_sysmeta(7, ""))
  expect_error(get_all_sysmeta(adc_test, ""))
  expect_error(get_all_sysmeta(adc_test, "urn:uuid:3e3bb5de-ec63-4f13-a549-813f0cf28610"))
  expect_error(get_all_sysmeta(adc_test, rm_pid, nmax = -7))
  expect_error(get_all_sysmeta(adc_test, rm_pid, child_packages = 7))

  all <- get_all_sysmeta(adc_test, rm_pid)

  expect_message(get_all_sysmeta(adc_test, rm_pid))
  expect_type(all, "list")
  expect_length(all, 5)
  expect_equal(names(all)[1], "dummy_resource_map.xml")

  expect_message(get_all_sysmeta(adc_test, "resource_map_urn:uuid:924f81f6-2e68-4eb8-925f-53f5b66318ec"))
})
