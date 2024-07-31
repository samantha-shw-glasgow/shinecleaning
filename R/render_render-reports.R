render_report <- function(survey_data = NULL,
                          school_name = NULL,
                          local_authority_name = NULL,
                          number_invited = NULL,
                          output_location = getwd(),
                          filename = "primary_report.docx") {
  render_env <- new.env()

  survey_data <- survey_data[grepl("^\\d", survey_data$StartDate), ]

  if (is.null(number_invited))
    number_invited <- nrow(survey_data)

  assign("input_data", survey_data, envir = render_env)

  rmarkdown::render(
    system.file("templates", "primary-reports/index.qmd", package = "SHINEcleaning"),
    output_dir = output_location,
    envir = render_env,
    output_file = filename
  )

}
