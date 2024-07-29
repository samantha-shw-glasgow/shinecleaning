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
  should_include <- ifelse(data$`Duration (in seconds)` >= 60, TRUE, FALSE)
  tibble::tibble(
    include = should_include,
    message = ifelse(should_include, "", "Duration too short")
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
