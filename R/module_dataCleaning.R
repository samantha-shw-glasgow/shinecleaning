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
    "Detect duplicate cases",
    "Detect recurring postcodes",
    "Detect partial cases",
    "Detect straightlining",
    "Detect age/year mismatch",
    "Suggest class when missing",
    "Exclude test responses",
    "Exclude non-consenting",
    "Highlight missing School ID",
    "Check valid date of birth"
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

      validator_functions <- c(
        "Detect duplicate cases" = duplicate_cases,
        "Detect recurring postcodes" = recurring_postcodes,
        "Detect partial cases" = partial_cases,
        "Detect straightlining" = straightlining,
        "Detect age/year mismatch" = age_year_mismatch,
        "Suggest class when missing" = suggest_missing_class,
        "Exclude test responses" = no_test_responses,
        "Exclude non-consenting" = no_consent,
        "Highlight missing School ID" = has_school_id,
        "Check valid date of birth" = valid_dob
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
