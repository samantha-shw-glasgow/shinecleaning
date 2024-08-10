#' Render a single report for school or local authority
#'
#' This function renders a report from a given data file.
#'
#' @param survey_data A clean dataframe of survey results
#' @param school_name If a school report, the school's name
#' @param local_authority_name If a local authority report, the LA's name (note not compatible with school report/name argument)
#' @param number_invited Number invited to complete the survey
#' @param output_location Location of file output (defaults to working directory)
#' @param filename Name of file to output
#'
#'
render_report <- function(survey_data = NULL,
                          school_name = NULL,
                          local_authority_name = NULL,
                          number_invited = NULL,
                          output_location = getwd(),
                          filename = "primary_report.docx") {
  render_env <- new.env()

  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("officedown", quietly = TRUE)

  survey_data <- survey_data[grepl("^\\d", survey_data$`StartDate`), ] |>
    data_prep()

  if (is.null(number_invited))
    number_invited <- nrow(survey_data)

  assign("input_data", survey_data, envir = render_env)

  rmarkdown::render(
    system.file("templates", "primary-reports/index.qmd", package = "SHINEcleaning"),
    output_dir = output_location,
    envir = render_env,
    output_file = filename,
    params = list(school_name = school_name)
  )

}


#' Data preparation phase
#'
#' These functions prepare the data by reshaping it and generating necessary variables.
#' Helper functions for variables are included.
#'
#' @param survey_data The data to process
#' @param report_type The type of survey data ("primary" or "secondary")
#'
#' @importFrom rlang .data
#'
#' @returns
#' `data_prep`: A dataframe with the required variables for rendering a report
#'
#' The score-calculating functions return the dataset with relevant columns appended:
#'
#' `who_score`:  WHO 5-item wellbeing score (`who_score` variable) and categorical breakdown (`who_cat`: low/good)
#'
#' `mm_score`: 'Me and My feelings' score for primary schools
#'
#' `sehs_primary`: SEHS score for primary schools
#'
#' `sehs_secondary`: SEHS score for secondary schools
#'
#' `asw_score`: Adolescent sleep-wake score for secondary schools
#'
#' `sdq_score`: SDQ score for secondary schools
#'
data_prep <- function(survey_data, report_type = "primary") {

  # This should create:
  #  - WHO5 wellbeing score - `who_score` and `who_cat`

  #  Primary:
  #  - 'Me and my feelings' scores - emotional and behavioural `mm_score`
  #  - 'Gratitude', 'Zest', 'Optimism', 'Persistence', 'Pro-social' - `sehs_primary`
  #  - Overall coviality `cov_score`
  # Secondary:
  #  - secondary sehs (averaging by categories)
  #  - strenths and difficulties score - `sdq_score`
  #  - Adolescent sleep wake score - `asw_score`
  #
  # It should also filter refusal to complete survey

  survey_out <- survey_data |>
    filter(.data$consent == "Yes, I am happy to take part") |>
    mutate(gender = .data$gender2) |>
    who_score()

  if (report_type == "primary") {

    if (!("mm1" %in% colnames(survey_out))) {
      stop("Dataset is missing expected variables for primary report. ",
           "Did you correctly specify report type and are columns correctly named?")
    }

    survey_out |>
      mm_score() |>
      sehs_primary()

  } else if (report_type == "secondary") {

    if (!("ASW1" %in% colnames(survey_out))) {
      stop("Dataset is missing expected variables for primary report. ",
           "Did you correctly specify report type and are columns correctly named?")
    }

    survey_out |>
      sehs_secondary() |>
      asw_score() |>
      sdq_score()

  } else {
    stop(glue::glue("\"{report_type}\" is not a valid report type. ",
                    "Specify \"primary\" or \"secondary\" to match data."))
  }

}

#' @rdname data_prep
who_score <- function(survey_data) {

  # Sum the score of the five `Who` variables and multiply by 4

  who_responses <- c(
    "At no time",
    "Some of the time",
    "Less than half of the time",
    "More than half of the time",
    "Most of the time",
    "All of the time"
  )

  survey_data |>
    mutate(across(starts_with("Who"), ~match(.x, who_responses) - 1)) |>
    mutate(who_score = rowSums(pick(starts_with("Who"))) * 4,
           who_cat = case_when(
             who_score <= 50 ~ "low",
             who_score > 50 ~ "good"
           ), .keep = "none") |>
    bind_cols(survey_data, x = _)

}

#' @rdname data_prep
mm_score <- function(survey_data) {

  mm_responses <- c(
    "Never",
    "Sometimes",
    "Always"
  )

  # Add pro-rata correction if <1/3 missing

  # mme - ten columns, up to 3 missing
  # mmb - six columns, up to 2 missing

  survey_data |>
    transmute(across(starts_with("mm"), ~match(.x, mm_responses) - 1),
           mm15 = 2L - mm15,
           mme_missing = rowSums(pick(mm1:mm10) |> is.na()),
           mmb_missing = rowSums(pick(mm11:mm16) |> is.na())
           ) |>
    mutate(
      mme_score = rowSums(pick(mm1:mm10), na.rm = TRUE),
      mmb_score = rowSums(pick(mm11:mm16), na.rm = TRUE),
      mme_score = case_when(
        mme_missing == 0 ~ mme_score,
        mme_missing <=3 ~ 10 * mme_score / (10 - mme_missing),
        mme_missing > 3 ~ NA_real_
      ),
      mmb_score = case_when(
        mmb_missing == 0 ~ mmb_score,
        mmb_missing <=2 ~ 6 * mmb_score / (6 - mmb_missing),
        mmb_missing > 2 ~ NA_real_
      ),
      mme_cat = if_else(mme_score <= 9, "Expected", "Elevated"),
      mmb_cat = if_else(mmb_score <= 5, "Expected", "Elevated"),
      .keep = "none"
      )  |>
    bind_cols(survey_data, x = _)

}

#' @rdname data_prep
sehs_primary <- function(survey_data) {
  survey_data
}

#' @rdname data_prep
sehs_secondary <- function(survey_data) {
  survey_data

}

#' @rdname data_prep
asw_score <- function(survey_data) {
  survey_data

}

#' @rdname data_prep
sdq_score <- function(survey_data) {
  survey_data

}

