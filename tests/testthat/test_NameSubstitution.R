#' test_NameSubstitution.R

test_that("Names are substituted in EML documents", {

  # Load the user data frame lookup table
  user_data_file <- '~/development/arctic-data/misc/table_security.user.csv'
  user_names <- read.csv(user_data_file)
  expect_gt(nrow(user_names), 400)

  # Convert an ISO document into EML for use
  iso_path <- '~/development/iso2eml/'
  #iso_file <- paste0(iso_path, 'tests/iso/iso-07.xml')
  #isotoeml <- xslt::read_xslt(paste0(iso_path, "src/iso2eml.xsl"))
  #eml_path <- convert_iso_to_eml(iso_file, isotoeml)
  # For now, just use an existing EML doc from the ISO conversion
  eml_path <- paste0(iso_path, 'build/eml-07.xml')
  expect_true(file.exists(eml_path))

  # Substitute the name data from the lookup table
  new_eml_path <- substitute_eml_party(eml_path)
  expect_true(file.exists(new_eml_path))

  # TODO: test that the new EML doc contains name structures
})
