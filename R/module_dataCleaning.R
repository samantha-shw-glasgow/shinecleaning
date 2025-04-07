#' dataCleaning UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
dataCleaningUI <- function(id) {
  ns <- NS(id)

  # The order here will reflect the order that the error messages appear
  # in the data cleaning spreadsheet. "Detect duplicate cases" is
  # deliberately first. The order of the other checks more or less reflects
  # importance.
  validator_options <- c(
    "Flag duplicate cases",
    "Flag recurring postcodes",
    "Flag/exclude partial cases",
    "Flag straightlining",
    "Flag age/year mismatch",
    "Suggest class when missing",
    "Exclude test responses",
    "Exclude non-consenting",
    "Flag missing School ID",
    "Flag invalid date of birth"
  )

  tagList(
    h2("Cleaning options"),
    checkboxGroupInput(
      ns("validator_selection"),
      label = span(
        "Select validators to run",
        bslib::tooltip(
          icon("info-circle"),
          "See the help section (top right) for information about the validator rules.",
          placement = "right"
        )),
      validator_options,
      selected = validator_options
    ),
    textOutput(ns("txt"))
  )
}

#' dataCleaning Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from rawUpload_server
#'
#' @keywords internal
dataCleaning_server <- function(id, data) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # When rules are added or modified, the descriptions in R\module_help.R should be updated accordingly.
      # For clarity:
      # - Rules that add a warning but don't automatically exclude rows are described as "Flag..."
      # - Rules that can automatically exclude rows are described as "Exclude..." or "Flag/exclude..."
      # - Exceptional rules with special behaviour can be described differently (e.g. "Suggest class...")
      validator_functions <- c(
        "Flag duplicate cases" = duplicate_cases,
        "Flag recurring postcodes" = recurring_postcodes,
        "Flag/exclude partial cases" = partial_cases,
        "Flag straightlining" = straightlining,
        "Flag age/year mismatch" = age_year_mismatch,
        "Flag missing School ID" = has_school_id,
        "Flag invalid date of birth" = valid_dob,
        "Flag responses outside school hours" = completed_outside_school_hours,
        "Flag responses at weekends" = completed_at_weekend,
        "Suggest class when missing" = suggest_missing_class,
        "Exclude test responses" = no_test_responses,
        "Exclude non-consenting" = no_consent
      )

      clean_data <- reactive({
        validators <- validator_functions[input$validator_selection]
        apply_cleaning_rules(data(), validators = validators) |>
          dplyr::mutate(
            completed_date = as.character(lubridate::parse_date_time(.data$RecordedDate, c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M")) |> as.Date()),
            date_of_birth = as.character(lubridate::ymd(paste(.data$dobyr, .data$dobmnth, .data$dobday), quiet = TRUE)),
            age = calculate_age(.data$RecordedDate, .data$dobyr, .data$dobmnth, .data$dobday))
      })

      return(clean_data) ## - must return reactive dataframe
    }
  )
}

# UI
# dataCleaningUI('id')

# server
# dataCleaning_server('id')
