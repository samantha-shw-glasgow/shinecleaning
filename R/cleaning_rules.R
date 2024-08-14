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

#' @rdname validators
suggest_missing_class <- function(data) {
  classes <- c(
    rep(NA, 3),
    c("P1", "P2", "P3", "P4", "P5", "P6", "P7"),
    c("S1", "S2", "S3", "S4", "S5", "S6")
  )
  messages <- data |>
    dplyr::mutate(
      dob_ym = lubridate::ym(paste(dobyr, dobmnth)),
      current_year = lubridate::dmy_hm(RecordedDate),
      school_dob = lubridate::year(dob_ym - months(8)),
      school_year = lubridate::year(current_year - months(7)),
      expected_class = classes[school_year - school_dob],
      missing = is.na(class) | class == "Prefer not to say",
      message = dplyr::case_when(
        missing & !is.na(expected_class)  ~ paste("Missing class, expected", expected_class),
        missing & is.na(expected_class)   ~ "Missing class",
        !missing                          ~ ""
      ),
    ) |>
    dplyr::pull(message)
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}
