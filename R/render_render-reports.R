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
render_report <- function(survey_data = NULL,
                          school_name = NULL,
                          local_authority_name = NULL,
                          number_invited = NULL,
                          output_location = getwd(),
                          filename = "primary_report.docx") {
  render_env <- new.env()

  survey_data <- survey_data[grepl("^\\d", survey_data$`Start Date`), ]

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
