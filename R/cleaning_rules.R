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
  relevant_cols <- select(
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

class_num_to_name <- function(num) {
  c(
    rep(NA, 3),
    c("P1", "P2", "P3", "P4", "P5", "P6", "P7"),
    c("S1", "S2", "S3", "S4", "S5", "S6")
  )[num]
}
class_name_to_num <- function(name) {
  num <- c(
    "P1" = 4, "P2" = 5, "P3" = 6, "P4" = 7, "P5" = 8, "P6" = 9, "P7" = 10,
    "S1" = 11, "S2" = 12, "S3" = 13, "S4" = 14, "S5" = 15, "S6" = 16
  )[name]
  names(num) <- NULL
  num
}
calculate_expected_class <- function(data) {
  data |>
    dplyr::mutate(
      dob_ym = lubridate::ym(paste(dobyr, dobmnth)),
      current_year = lubridate::dmy_hm(RecordedDate),
      school_dob = lubridate::year(dob_ym - months(8)),
      school_year = lubridate::year(current_year - months(7)),
      expected_class_num = school_year - school_dob,
      expected_class_name = class_num_to_name(expected_class_num)
    )
}

#' @rdname validators
suggest_missing_class <- function(data) {
  messages <- data |>
    calculate_expected_class() |>
    dplyr::mutate(
      missing = is.na(class) | class == "Prefer not to say",
      message = dplyr::case_when(
        missing & !is.na(expected_class_name) ~ paste("Missing class, expected", expected_class_name),
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
      class_num = class_name_to_num(class),
      message = ifelse(
        class_num < expected_class_num - 1 | class_num > expected_class_num + 1,
        paste0("Unexpected class (", expected_class_name, " predicted)"),
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
straightlining <- function(data) {
  prefixes <- c(
    "asw",
    "lifesat",
    "mm",
    "sdq",
    "sehs",
    "who"
  )
  for (prefix in prefixes) {
    results <- data |>
      dplyr::select(starts_with(prefix)) |>
      # Check if all columns are equal - see https://stackoverflow.com/a/76973366
      dplyr::mutate(all_equal = apply(dplyr::pick(dplyr::everything()), 1, dplyr::n_distinct, na.rm = T) == 1) |>
      dplyr::pull(all_equal)
    data[[paste0("_straightline_", prefix)]] <- results
  }
  messages <- data |>
    dplyr::select(dplyr::starts_with("_straightline_")) |>
    apply(1, any) |>
    ifelse("Straightlining detected", "")
  tibble::tibble(
    include = TRUE,
    message = messages
  )
}

#' @rdname validators
duplicate_postcodes <- function(data) {
  postcode_count <- data |>
    dplyr::add_count(postcode_5_TEXT) |>
    dplyr::pull(n)
  tibble::tibble(
    include = TRUE,
    message = ifelse(
      postcode_count >= 5,
      paste("Postcode occurs", postcode_count, "times"),
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
