#' Validate Property Identification Numbers (PIDs)
#'
#' This function validates a given input of Property Identification Numbers (PIDs) against a list of valid PIDs.
#'
#' @param pid_input A character vector or single string containing the PIDs to validate. Multiple PIDs can be separated by commas or spaces.
#' @param valid_pids A character vector of valid PIDs to check against.
#' @param enable_check A logical value indicating whether validation should be performed. Defaults to `TRUE`.
#'
#' @return Returns `NULL` if all PIDs are valid. If invalid, returns an error message as a string specifying the issue.
#'
#' @details
#' The function performs the following validation checks:
#' - Ensures each PID is exactly 8 digits long.
#' - Ensures each PID contains only numeric characters (0-9).
#' - Checks if each PID exists in the provided list of valid PIDs.
#'
#' If validation is disabled (`enable_check = FALSE`), the function will return `NULL` without performing any checks.
#'
#' @examples
#' # Example usage:
#' valid_pids <- c("12345678", "87654321")
#' validate_pid_input("12345678, 87654321", valid_pids)
#' validate_pid_input("12345678, 00000000", valid_pids)
#'
#' @export
validate_pid_input <- function(pid_input, valid_pids, enable_check = TRUE) {
  if (!enable_check) {
    return(NULL)
  }

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
