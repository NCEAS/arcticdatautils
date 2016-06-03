get_award_info <- function(award) {
  # Convert a numeric award # to a character vector
  if (is.numeric(award)) {
    award <- as.character(award)
  }

  # Do a basic validation of the award format
  stopifnot(is.character(award),
            length(award) == 1,
            nchar(award) == 7)

  url <- paste0("http://api.nsf.gov/services/v1/awards/", award, ".json?printFields=title,startDate,expDate,abstractText,piFirstName,piMiddleInitial,piLastName,piEmail")
  response <- httr::GET(url)
}

create_eml_project <- function(award_info) {

}
