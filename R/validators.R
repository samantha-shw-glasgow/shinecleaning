#' Validators
#' @param data Raw input dataset
#' @name validators
#' @returns A character vector of validation messages, in the same order as the rows in the input data
NULL

#' @rdname validators
check_duplicate_dob <- function(data) {
  occurrences_lookup <- table(data$dob)
  occurrences <- c(occurrences_lookup[as.character(data$dob)])
  output <- ifelse(occurrences > 1, "Duplicate DOB", "")
  names(output) <- NULL
  output
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
