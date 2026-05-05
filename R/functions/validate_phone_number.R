validate_phone_number <- function(number) {
  if (!str_detect(number, "^\\d{3}-\\d{3}-\\d{4}$")) {
    "Phone number must be in format 902-456-7890"
  } else {
    return(NULL)
  }
}
