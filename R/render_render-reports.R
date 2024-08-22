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
render_report <- function(survey_data,
                          school_name = NA,
                          local_authority_name = NA,
                          term = NULL,
                          number_invited = NULL,
                          output_location = getwd(),
                          gender_split = TRUE,
                          filename = "primary_report.docx") {

  if(!is.na(school_name) && !is.na(local_authority_name)) {
    stop("Is this a school or Local Authority report?\n",
         "Please provide only `school_name` or `local_authority_name`")
  } else if(is.na(school_name) && is.na(local_authority_name)) {
    stop("Please provide a `school_name` or a `local_authority_name`")
  }

  render_env <- new.env()

  survey_data <- survey_data[grepl("^\\d", survey_data$`StartDate`), ] |>
    data_prep()

  report_name <- if_else(!is.na(school_name), school_name, local_authority_name)

  is_la <- is.na(local_authority_name)

  if (is.null(number_invited))
    number_invited <- nrow(survey_data)

  assign("input_data", survey_data, envir = render_env)

  rmarkdown::render(
    system.file("templates", "primary-reports/index.qmd", package = "SHINEcleaning"),
    output_dir = output_location,
    envir = render_env,
    output_file = filename,
    params = list(
      is_la_report = FALSE,
      school_name = report_name,
      term = term,
      number_invited = number_invited,
      gender_split = gender_split
      )
  )

}


data_prep <- function(survey_data) {

  survey_data |>
    filter(consent == "Yes, I am happy to take part") |>
    mutate(gender = gender2)

  #' This should create:
  #'  - WHO5 wellbeing score
  #'  - lifesat overall?
  #'  - 'Me and my feelings' scores - emotional and behavioural
  #'  - 'Gratitude', 'Zest', 'Optimism', 'Persistence', 'Pro-social'
  #'  - Overall coviality
  #'
  #' It should also filter refusal to complete survey

}
