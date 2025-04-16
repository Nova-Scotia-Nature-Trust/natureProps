#' Validate PID Input
#'
#' @param pid_input The input value to validate
#' @param valid_pids Character vector of valid PIDs to check against
#' @param enable_check Logical whether to enable validation (default TRUE)
#'
#' @return NULL if valid, error message string if invalid
validate_pid_input <- function(pid_input, valid_pids, enable_check = TRUE) {
  if (!enable_check) return(NULL)
  
  # If empty input, return valid
  if (is.null(pid_input) || length(pid_input) == 0 || all(pid_input == "")) {
    return(NULL)
  }
  
  # Handle comma/space separated input
  if (length(pid_input) == 1 && grepl("[,\\s]", pid_input)) {
    codes <- unlist(strsplit(pid_input, "[,\\s]+"))
  } else {
    codes <- pid_input
  }
  
  codes <- trimws(codes)
  
  # Validation checks
  if (any(nchar(codes) != 8)) {
    return("Each PID must be exactly 8 digits.")
  }
  
  if (any(!grepl("^[0-9]{8}$", codes))) {
    return("Each PID must contain only digits (0-9).")
  }
  
  invalid_codes <- codes[!codes %in% valid_pids]
  if (length(invalid_codes) > 0) {
    return(sprintf(
      "The following PID(s) are invalid (missing from PRD): %s",
      paste(invalid_codes, collapse = ", ")
    ))
  }
  
  NULL
}