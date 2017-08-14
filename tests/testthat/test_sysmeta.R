context("sysmeta")

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
