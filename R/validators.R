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
duration_too_short <- function(data) {
  passes <- data$`Duration (in seconds)` >= 60
  tibble::tibble(
    include = TRUE,
    message = ifelse(passes, "", "Duration too short")
  )
}

#' @rdname validators
no_test_responses <- function(data) {
  should_include <- dplyr::case_match(
    data$Status,
    "IP Address"      ~ TRUE,
    "Survey Preview"  ~ FALSE,
    .default          = FALSE
  )
  message <- dplyr::case_match(
    data$Status,
    "IP Address"      ~ "",
    "Survey Preview"  ~ "Preview response",
    .default          = "Unexpected response type"
  )
  tibble::tibble(
    include = should_include,
    message = message
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
