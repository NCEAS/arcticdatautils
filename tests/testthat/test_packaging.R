#' test_packaging.R

context("packaging")

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


test_that("extra triple can be added to a resource map", {
  path <- generate_resource_map("metadata", "data",
                                other_statements = data.frame(subject="http://example.com/me",
                                                              predicate="http://example.com/is_related_to",
                                                              object="http://example.com/myself"))
  statements <- parse_resource_map(path)
  expect_true("<http://example.com/me>" %in% statements$subject)
})

test_that("extra statements are maintained between updates",{

})
