validate_postal_code <- function(code) {
  # Accepts Canadian (A1A 1A1 / A1A1A1) or US (12345 / 12345-6789) formats
  if (!str_detect(code, "^[A-Za-z]\\d[A-Za-z] ?\\d[A-Za-z]\\d$|^\\d{5}(-\\d{4})?$")) {
    "Postal code must be a valid Canadian (A1A 1A1) or US (12345) format"
  } else {
    return(NULL)
  }
}
