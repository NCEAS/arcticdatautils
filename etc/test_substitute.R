devtools::load_all(".")

files <- paste0("~/src/iso2eml/build/", dir("~/src/iso2eml/build/"))

for (file in files) {
  print(file)
  tmp <- tempfile()
  file.copy(file, tmp, overwrite = TRUE)

  substitute_eml_party(tmp)
}
