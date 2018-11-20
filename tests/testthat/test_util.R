context("Utilities")

test_that("paths can be joined", {
  expect_equal(path_join(""), "")
  expect_equal(path_join(1), "1")
  expect_equal(path_join(NULL), "")
  expect_equal(path_join(c("./", "./test")), "./test")

  # Test that it handles variables and not just literal values
  part_one <- "./"
  part_two <- "./asdf"
  expect_equal(path_join(c(part_one, part_two)), "./asdf")
  expect_equal(path_join(c(part_one, "./", part_two)), "./asdf")

  # Other tests
  expect_equal(path_join("~/src/arcticdata./inst/asdf"), "~/src/arcticdata/inst/asdf")
})

test_that('we can set public READ on all versions of a data package', {
  cn <- dataone::CNode('STAGING')
  mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')
  if (!is_token_set(mn)) {
    skip("No token set. Skipping test.")
  }

  pkg <- create_dummy_package(mn)
  remove_public_read(mn, unlist(pkg))
  pkg_v2 <- publish_update(mn, pkg$metadata, pkg$resource_map, pkg$data, public = FALSE)
  # Set public read on all versions
  set_public_read_all_versions(mn, pkg$resource_map)
  pids <- c(unlist(pkg), unlist(pkg_v2))

  expect_true(all(is_public_read(mn, pids)))
})
