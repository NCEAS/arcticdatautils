context("sysmeta")

test_that("the replication policy gets cleared", {
  library(datapack)

  sysmeta <- new("SystemMetadata")
  expect_true(sysmeta@replicationAllowed)

  sysmeta <- clear_replication_policy(sysmeta)
  expect_false(sysmeta@replicationAllowed)
})
