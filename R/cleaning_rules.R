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
partial_cases <- function(data) {
  relevant_cols <- dplyr::select(
    data,
    dplyr::starts_with("oops"),
    dplyr::starts_with("activity"),
    dplyr::starts_with("asw"),
    dplyr::starts_with("class"),
    dplyr::starts_with("fas"),
    dplyr::starts_with("health"),
    dplyr::starts_with("lifesat"),
    dplyr::starts_with("loneliness"),
    dplyr::starts_with("mm"),
    dplyr::starts_with("sch"),
    dplyr::starts_with("sdq"),
    dplyr::starts_with("sehs"),
    dplyr::starts_with("selfh"),
    dplyr::starts_with("who")
  )
  relevant_cols <- replace(relevant_cols, relevant_cols == "Prefer not to say", NA)
  n_missing_cols <- rowSums(is.na(relevant_cols))
  tibble::tibble(
    include = TRUE,
    message = ifelse(
      n_missing_cols > 0.5 * ncol(relevant_cols),
      paste0("Missing ", n_missing_cols, "/", ncol(relevant_cols), " answers"),
      ""
    )
  )
}

#' @rdname validators
duplicate_cases <- function(data) {
  if (!("sex" %in% colnames(data))) data$sex <- NA_character_

  messages <- data |>
    dplyr::group_by(
      .data$sex,
      .data$gender,
      .data$dobmnth,
      .data$dobday,
      .data$dobyr,
      .data$`School ID code`
    ) |>
    dplyr::mutate(
      duplicate_n = dplyr::n(),
      message = ifelse(
        .data$duplicate_n > 1 & !is.na(.data$gender) & !is.na(.data$dobmnth) & !is.na(.data$dobday) &
          !is.na(.data$dobyr) & !is.na(.data$`School ID code`),
        paste("Possible duplicates:", paste0(sort(.data$ResponseId), collapse = ", ")),
        ""
      )
    ) |>
    dplyr::pull(message)
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}

class_lookup <- tibble::tribble(
  ~school_age, ~class,
  5, "P1",
  6, "P2",
  7, "P3",
  8, "P4",
  9, "P5",
  10, "P6",
  11, "P7",
  12, "S1",
  13, "S2",
  14, "S3",
  15, "S4",
  16, "S5",
  17, "S6"
)
calculate_expected_class <- function(data) {
  data |>
    dplyr::mutate(
      dob = dplyr::case_when(
        is.na(dobyr) ~ NA,
        is.na(dobmnth) ~ lubridate::make_date(dobyr, 6, 1),
        TRUE ~ lubridate::ym(paste(dobyr, dobmnth), quiet = TRUE),
      ),
      current_year = lubridate::parse_date_time(.data$RecordedDate, c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M")),
      school_birthyear = lubridate::year(.data$dob - months(2)),
      current_year = lubridate::year(.data$current_year |> lubridate::floor_date("months") - months(7)),
      school_age = .data$current_year - .data$school_birthyear
    ) |>
    dplyr::left_join(class_lookup, by = "school_age") |>
    dplyr::rename(class = "class.x", expected_class_name = "class.y") |>
    dplyr::left_join(class_lookup, by = "class") |>
    dplyr::rename(school_age = "school_age.x", school_age_based_on_class = "school_age.y")
}

#' @rdname validators
suggest_missing_class <- function(data) {
  messages <- data |>
    calculate_expected_class() |>
    dplyr::mutate(
      missing = is.na(class) | class == "Prefer not to say",
      message = dplyr::case_when(
        missing & !is.na(expected_class_name) ~ paste0("Missing class (expected ", expected_class_name, ")"),
        missing & is.na(expected_class_name)  ~ "Missing class",
        !missing                              ~ ""
      ),
    ) |>
    dplyr::pull(message)
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}

#' @rdname validators
age_year_mismatch <- function(data) {
  messages <- data |>
    calculate_expected_class() |>
    dplyr::mutate(
      message = dplyr::case_when(
        school_age_based_on_class > school_age + 1 &
          school_age < 5 ~ "Unexpected class (below school age)",
        school_age_based_on_class < school_age - 1 &
          school_age > 17 ~ "Unexpected class (above school age)",
        school_age_based_on_class < school_age - 1 |
          school_age_based_on_class > school_age + 1 ~
          paste0("Unexpected class (", expected_class_name, " predicted)"),
        TRUE ~ ""
      )
    ) |>
    dplyr::pull(message)
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}

#' @rdname validators
straightlining <- function(data) {
  prefixes <- c(
    "asw",
    "lifesat",
    "mm",
    "sdq",
    "sehs",
    "who"
  )
  failed_prefixes <- rep("", nrow(data))
  for (prefix in prefixes) {
    results <- data |>
      dplyr::select(dplyr::starts_with(prefix)) |>
      # Check if all columns are equal - see https://stackoverflow.com/a/76973366
      dplyr::mutate(all_equal = apply(dplyr::pick(dplyr::everything()), 1, dplyr::n_distinct, na.rm = T) == 1) |>
      dplyr::pull(.data$all_equal)
    failed_prefixes <- ifelse(
      results,
      paste0(failed_prefixes, ", ", prefix),
      failed_prefixes
    )
  }

  failed_prefixes <- stringr::str_remove(failed_prefixes, "^, ")
  messages <- ifelse(
    failed_prefixes != "",
    paste0("Straightlining detected (", failed_prefixes, ")"),
    ""
  )

  tibble::tibble(
    include = TRUE,
    message = messages
  )
}

#' @rdname validators
recurring_postcodes <- function(data) {
  data_with_count <- data |>
    # Convert to uppercase, remove whitespace at the start and end,
    # and make sure there's only one space in the middle
    dplyr::mutate(
      clean_postcode = .data$postcode_5_TEXT |>
        toupper() |>
        stringr::str_trim() |>
        stringr::str_replace_all("  *", " ")
    ) |>
    dplyr::add_count(.data$clean_postcode)

  tibble::tibble(
    include = TRUE,
    message = ifelse(
      !is.na(data_with_count$clean_postcode) &
      data_with_count$clean_postcode != "" &
        data_with_count$n >= 5,
      paste("Postcode occurs", data_with_count$n, "times"),
      ""
    )
  )
}

#' @rdname validators
no_consent <- function(data) {
  has_consented <- grepl("Yes", data$consent)
  tibble::tibble(
    include = has_consented,
    message = ifelse(has_consented, "", "Consent not given")
  )
}

#' @rdname validators
has_school_id <- function(data) {
  has_no_id <- is.na(data$`School ID code`)
  tibble::tibble(
    include = TRUE,
    message = ifelse(has_no_id, "School ID missing", "")
  )
}

#' @rdname validators
valid_dob <- function(data) {
  dob <- lubridate::ymd(paste(data$dobyr, data$dobmnth, data$dobday), quiet = TRUE)
  invalid_dob <- is.na(dob)
  # Alternative check - if a DOB is entered BUT not valid
  # invalid_dob <- is.na(dob) & (!is.na(dobyr) | !is.na(dobmnth) | !is.na(dobday))
  tibble::tibble(
    include = TRUE,
    message = ifelse(invalid_dob, "Invalid or missing DOB", "")
  )
}
