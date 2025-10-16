#' Render a single report for school or local authority
#'
#' This function renders a report from a given data file.
#'
#' @param survey_data A clean dataframe of survey results
#' @param survey_type The report type - 'primary'/'secondary'
#' @param school_name If a school report, the school's name
#' @param local_authority_name If a local authority report, the LA's name (note not compatible with school report/name argument)
#' @param cluster_label Type of school grouping, e.g. local authority, primary cluster. Only used for 'local authority' type reports
#' @param term Name of term to print on report
#' @param number_invited Number invited to complete the survey
#' @param output_location Location of file output (defaults to working directory)
#' @param gender_split To split report outputs by gender
#' @param classes Vector or nested list of classes (clustered by combinations)
#' @param filename Name of file to output
#'
#'
render_report <- function(survey_data,
                          survey_type,
                          school_name = NA,
                          local_authority_name = NA,
                          cluster_label = NULL,
                          term = NULL,
                          number_invited = NULL,
                          output_location = NULL,
                          gender_split = TRUE,
                          classes = NULL,
                          filename = "primary_report.docx") {

  requireNamespace("officedown")
  requireNamespace("rsvg")
  requireNamespace("waldo")

  if (survey_type == "primary") {
    template <- "primary-reports/index.qmd"
  } else if (survey_type == "secondary") {
    template <- "secondary-reports/index.qmd"
  } else {
    warning(glue::glue("\"{survey_type}\" is not a known type of survey"))
    stop("Please specify 'primary' or 'secondary' as `survey_type`")
  }


  if (!is.na(school_name) && !is.na(local_authority_name)) {
    stop(
      "Is this a school or Local Authority report?\n",
      "Please provide only `school_name` or `local_authority_name`"
    )
  } else if (is.na(school_name) && is.na(local_authority_name)) {
    stop("Please provide a `school_name` or a `local_authority_name`")
  }


  render_env <- new.env()

  survey_data <- survey_data[grepl("^\\w", survey_data$consent), ] |>
    data_prep(report_type = survey_type)

  report_name <- dplyr::if_else(!is.na(school_name), school_name, local_authority_name)

  is_la <- !is.na(local_authority_name)

  if (is.null(number_invited)) {
    number_invited <- nrow(survey_data)
  }

  assign("input_data", survey_data, envir = render_env)

  params <- list(
      is_la_report = is_la,
      school_name = report_name,
      term = term,
      number_invited = number_invited,
      gender_split = gender_split
    )

  if (!is.null(classes)) params$classes <- classes

  if (is_la) params$cluster_label <- cluster_label

  rmarkdown::render(
    system.file("templates", template, package = "SHINEcleaning"),
    output_dir = output_location,
    envir = render_env,
    output_file = filename,
    params = params
  )
}
