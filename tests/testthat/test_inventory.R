#' test_inventory.R

test_that("an inventory can be created correctly", {
  x <- inv_init()
  expect_true(is.data.frame(x))
  expect_true(nrow(x) == 0)
})

test_that("an inventory can be populated with files", {
  # Case 1: Empty inv, non-empty file
  inv <- inv_init()
  result <- inv_load_files(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "files_simple.txt"), inv)
  expect_true(nrow(result) == 3)

  # Case 2: Non-empty inv, non-empty file, where some are the same
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  result <- inv_load_files(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "files_simple.txt"), inv)
  expect_true(nrow(result) == 5)

  # Case 3: Non-empty inv, non-empty file, where none are the same
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  inv <- subset(inv, file != "C")
  result <- inv_load_files(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "files_simple.txt"), inv)
  expect_true(nrow(result) == 5)

  # Case 4: Non-empty inv, non-empty file, inventory already has columns
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  inv$bytes <- rep(1000, nrow(inv))
  result <- inv_load_files(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "files_simple.txt"), inv)

  expect_true(nrow(result) == 5)
  expect_true(ncol(result) == 2)
  expect_true(length(table(result$bytes, exclude=NULL)) == 2)
})

test_that("an inventory can be populated with byte sizes", {
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  result <- inv_load_sizes(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "sizes_simple.txt"), inv)

  expect_true(nrow(result) == 3)
  expect_true(ncol(result) == 2)
})

test_that("an inventory can be populated with checksums", {
  # Test if we can populate with checksums before sizes
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  inv_with_checksums <- inv_load_checksums(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "checksums_simple.txt"), inv)
  inv_with_both <- inv_load_sizes(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "sizes_simple.txt"), inv_with_checksums)

  expect_true(nrow(inv_with_both) == 3)
  expect_true(ncol(inv_with_both) == 3)

  # Test if we can populate with sizes before checksums
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  inv_with_sizes <- inv_load_sizes(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "sizes_simple.txt"), inv)
  inv_with_both <- inv_load_checksums(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "checksums_simple.txt"), inv_with_sizes)

  expect_true(nrow(inv_with_both) == 3)
  expect_true(ncol(inv_with_both) == 3)
})

# We should be able to call the same function multiple times and not break things
test_that("calling things repeatedly does not break things", {
  inv <- read.csv(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "inventory_simple.csv"), stringsAsFactors = FALSE)
  inv <- inv_load_checksums(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "checksums_simple.txt"), inv)
  inv$test <- NA
  inv <- inv_load_checksums(file.path(system.file("tests", "data", "inventory", package = "arcticdata"), "checksums_simple.txt"), inv)

  expect_true(ncol(inv) == 3)
  expect_true(nrow(inv) == 3)
})

test_that("an inventory can be updated with new information", {
  test_inv <- data.frame(file = "A", pid="", created = FALSE, stringsAsFactors = FALSE)
  new_inv <- data.frame(file = "A", pid="pidA", created = TRUE, stringsAsFactors = FALSE)
  result <- update_inventory(test_inv, new_inv)

  expect_true(result[1,"pid"] == "pidA")
  expect_true(result[1,"created"])
})
