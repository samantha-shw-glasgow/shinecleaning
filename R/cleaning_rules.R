apply_cleaning_rules <- function(data, validators) {
  messages <- rep("", nrow(data))
  should_include <- rep(TRUE, nrow(data))
  for (validator_fn in validators) {
    result <- validator_fn(data)
    messages <- append_if_nonempty(
      messages,
      result$message
    )
    should_include <- should_include & result$include
  }
  data |>
    dplyr::mutate(
      "Error messages" = messages,
      "Keep row?" = as.numeric(should_include),
      "Reviewer notes" = "",
      .before = 1
    )
}

append_if_nonempty <- function(string_1, string_2) {
  dplyr::case_when(
    string_1 == "" & string_2 == "" ~ "",
    string_1 != "" & string_2 == "" ~ string_1,
    string_1 == "" & string_2 != "" ~ string_2,
    string_1 != "" & string_2 != "" ~ paste(string_1, string_2, sep = "; "),
  )
}

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
duplicate_cases <- function(data) {
  messages <- data |>
    dplyr::group_by(
      gender1,
      gender2,
      dobmnth,
      dobday,
      dobyr,
      `School ID`
    ) |>
    dplyr::arrange(ResponseId) |>
    dplyr::mutate(
      duplicate_n = dplyr::n(),
      message = ifelse(
        duplicate_n > 1,
        paste("Possible duplicates:", paste0(ResponseId, collapse = ", ")),
        ""
      )
    ) |>
    dplyr::pull(message)
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}
