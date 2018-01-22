#' test_inventory.R

context("inventory")

test_that("an inventory can be created correctly", {
  x <- inv_init()
  expect_true(is.data.frame(x))
  expect_true(nrow(x) == 0)
})


test_that("an inventory can be updated with new information", {
  test_inv <- data.frame(file = "A", pid="", created = FALSE, stringsAsFactors = FALSE)
  new_inv <- data.frame(file = "A", pid="pidA", created = TRUE, stringsAsFactors = FALSE)
  result <- inv_update(test_inv, new_inv)

  expect_true(result[1,"pid"] == "pidA")
  expect_true(result[1,"created"])
})

test_that("hierarchical packages are correctly marked", {
  # Simple A->B->C relationship
  inv <- data.frame(file = c("./acadis-field-projects/SOMEPROJ/some_dataset/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/some_dataset/child/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/some_dataset/child/subchild/iso.xml"),
                    package = c("A", "B", "C"),
                    is_metadata = TRUE,
                    depth = c(5, 6, 7),
                    stringsAsFactors = FALSE)

  inv <- inv_add_parent_package_column(inv)

  expect_equal(inv$parent_package, c("", "A", "B"))


  # No parent packages
  inv <- data.frame(file = c("./acadis-field-projects/SOMEPROJ/some_dataset/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/another/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/yet_another/iso.xml"),
                    package = c("A", "B", "C"),
                    is_metadata = TRUE,
                    depth = c(5, 5, 5),
                    stringsAsFactors = FALSE)

  inv <- inv_add_parent_package_column(inv)

  expect_equal(inv$parent_package, c("", "", ""))

  # Slightly complex
  inv <- data.frame(file = c("./acadis-field-projects/SOMEPROJ/some_dataset/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/some_dataset/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/some_dataset/iso.xml",
                             "./acadis-gateway/ANOTHER/some_dataset/iso.xml",
                             "./acadis-gateway/YETANOTHER/a_nice_dataset/iso.xml",
                             "./acadis-gateway/YETANOTHER/another_nice_dataset/iso.xml",
                             "./acadis-gateway/YETANOTHER/a_nice_dataset/a_child_dataset/iso.xml"),
                    package = c("A", "B", "C", "D", "E", "F", "G"),
                    is_metadata = TRUE,
                    depth = c(5, 5, 5, 5, 5, 5, 6),
                    stringsAsFactors = FALSE)

  inv <- inv_add_parent_package_column(inv)

  expect_equal(inv$parent_package, c("", "", "", "", "", "", "E"))


  # Really deep nesting
  inv <- data.frame(file = c("./acadis-field-projects/SOMEPROJ/A/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/D/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/D/E/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/D/E/F/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/D/E/F/G/iso.xml",
                             "./acadis-field-projects/SOMEPROJ/A/B/C/D/E/F/G/H/iso.xml"),
                    package = c("A", "B", "C", "D", "E", "F", "G", "H"),
                    is_metadata = TRUE,
                    depth = c(5, 6, 7, 8, 9, 10, 11, 12),
                    stringsAsFactors = FALSE)

  inv <- inv_add_parent_package_column(inv)

  expect_equal(inv$parent_package, c("", "A", "B", "C", "D", "E", "F", "G"))

  # Another example
  inv <- data.frame(file = c("./acadis-gateway/some_long_name/parent.xml",
                             "./acadis-gateway/some_long_name/child_one/child.xml",
                             "./acadis-gateway/some_long_name/child_two/child.xml",
                             "./acadis-gateway/some_long_name/child_three/child.xml"),
                    package = c("A", "B", "C", "D"),
                    is_metadata = TRUE,
                    depth = c(4, 5, 5, 5),
                    stringsAsFactors = FALSE)

  inv <- inv_add_parent_package_column(inv)

  expect_equal(inv$parent_package, c("", "A", "A", "A"))
})
