#' dataCleaning UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
dataCleaningUI <- function(id) {
    ns <- NS(id)

    validator_options <- c(
      "Exclude test responses" = "no_test_responses",
      "Flag duration < 60 seconds" = "duration_too_short"
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

            clean_data <- reactive({
              validators <- lapply(input$validator_selection, get)
              run_validations(data(), validators = validators)
            })

            return(clean_data) ## - must return reactive dataframe
        }
    )
}

# UI
# dataCleaningUI('id')

# server
# dataCleaning_server('id')
