#' test_environment.R
#'
#' Test functions related to loading handling application environment.


test_that("can load a simple environment file", {
  x <- yaml::yaml.load_file(file.path(system.file("tests", "data", package = "arcticdata"), "test_environment.yml"))

  expect_true(length(x) == 3)
  expect_true(length(setdiff(c("development", "test", "production"), names(x))) == 0)
})


test_that("an environment string can be returned", {
  expect_is(env_get(), "character")
  expect_true(nchar(env_get()) > 0)
})

test_that("can correctly load the environment", {
  Sys.setenv("ARCTICDATA_ENV" = "")

  # Defaults to development if the env var isn't found
  expect_true(env_get() == "development")

  Sys.setenv("ARCTICDATA_ENV" = "production")
  expect_true(env_get() == "production")
})
