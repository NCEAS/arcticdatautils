#' test_packaging.R

test_that("child pids are correctly determined", {
  inventory <- data.frame(pid = c("A", "B", "C"),
                          package = c("PA", "PB", "PC"),
                          parent_package = c("", "A", "B"),
                          is_metadata = rep(TRUE, 3),
                          stringsAsFactors = FALSE)

  expect_equal(determine_child_pids(inventory, "A"), "resource_map_B")
  expect_equal(determine_child_pids(inventory, "B"), "resource_map_C")
  expect_equal(determine_child_pids(inventory, "C"), NULL)
})
