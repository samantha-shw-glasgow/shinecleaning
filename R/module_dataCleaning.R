#' dataCleaning UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
dataCleaningUI <- function(id) {
    ns <- NS(id)

    validator_options <- c(
      "Exclude test responses",
      "Exclude non-consenting",
      "Detect partial cases",
      "Detect duplicate cases",
      "Detect duplicate postcodes",
      "Detect age/year mismatch",
      "Detect straightlining",
      "Suggest class when missing"
    )

    tagList(
        h2("Cleaning options"),
        checkboxGroupInput(
            ns("validator_selection"), "Validators to run:",
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
              "Exclude test responses" = no_test_responses,
              "Exclude non-consenting" = no_consent,
              "Detect partial cases" = partial_cases,
              "Detect duplicate cases" = duplicate_cases,
              "Detect duplicate postcodes" = duplicate_postcodes,
              "Detect age/year mismatch" = age_year_mismatch,
              "Detect straightlining" = straightlining,
              "Suggest class when missing" = suggest_missing_class
            )

            clean_data <- reactive({
              validators <- validator_functions[input$validator_selection]
              apply_cleaning_rules(data(), validators = validators)
            })

            return(clean_data) ## - must return reactive dataframe
        }
    )
}

# UI
# dataCleaningUI('id')

# server
# dataCleaning_server('id')
