#' Validators
#' @param data Raw input dataset
#' @name validators
#'
#' @returns
#' A tibble with the same length as the input data, showing the validation
#' results for the corresponding rows in the input data. Columns:
#' * `message`: error message or an empty string
NULL

#' @rdname validators
check_duplicate_dob <- function(data) {
  occurrences_lookup <- table(data$dob)
  occurrences <- c(occurrences_lookup[as.character(data$dob)])
  messages <- ifelse(occurrences > 1, "Duplicate DOB", "")
  names(messages) <- NULL
  tibble::tibble(
    message = messages
  )
}

#' @rdname validators
nothing_is_wrong <- function(data) {
  rep("", nrow(data))
}

#' @rdname validators
everything_sucks <- function(data) {
  rep("This sucks", nrow(data))
}

#' @rdname validators
everything_is_fine <- function(data) {
  rep("This is fine", nrow(data))
}
