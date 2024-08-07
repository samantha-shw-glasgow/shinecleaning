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
#' @import officedown
#' @import tidyverse
#'
render_report <- function(survey_data = NULL,
                          school_name = NULL,
                          local_authority_name = NULL,
                          number_invited = NULL,
                          output_location = getwd(),
                          filename = "primary_report.docx") {
  render_env <- new.env()

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
#'
#' @returns
#' `data_prep`: A dataframe with the required variables for rendering a report
#'
#' `who_score`:  WHO 5-item wellbeing score (`who_score` variable) and categorical breakdown (`who_cat`: low/good)
#'
#' `mm_score`: 'Me and My feelings' score
data_prep <- function(survey_data) {

  survey_data |>
    filter(consent == "Yes, I am happy to take part") |>
    mutate(gender = gender2)

  # This should create:
  #  - WHO5 wellbeing score
  #  - 'Me and my feelings' scores - emotional and behavioural
  #  - 'Gratitude', 'Zest', 'Optimism', 'Persistence', 'Pro-social'
  #  - Overall coviality
  #
  # It should also filter refusal to complete survey

}

#' @rdname data_prep
who_score <- function(survey_data) {

  # Sum the score of the five `Who` variables and multiply by 4

  who_responses <- c(
    "At no time" = 0,
    "Some of the time" = 1,
    "Less than half of the time" = 2,
    "More than half of the time" = 3,
    "Most of the time" = 4,
    "All of the time" = 5
  )

  survey_data |>
    mutate(across(starts_with("Who"), ~who_responses[.x])) |>
    rowwise() |>
    mutate(who_score = sum(c_across(starts_with("Who"))) * 4,
           who_cat = case_when(
             who_score <= 50 ~ "low",
             who_score > 50 ~ "good"
           )) |>
    ungroup()

}

#' @rdname data_prep
mm_score <- function(survey_data) {

}

#' @rdname data_prep
sehs_primary <- function(survey_data) {

}

#' @rdname data_prep
sehs_secondary <- function(survey_data) {

}

