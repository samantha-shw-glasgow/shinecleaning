#' rawUpload UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUploadUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Upload data from Qualtrics"),
    fileInput(ns("upload"),
      label = span(
        "Select survey data",
        bslib::tooltip(
          icon("info-circle"),
          "See the help section (top right) for guidance on downloading data from Qualtrics in the correct format.",
          placement = "right"
        )
      ),
      buttonLabel = "Upload...",
      width = "100%",
      accept = ".csv"
    ),
    uiOutput(ns("warn"))
  )
}

#' rawUpload Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUpload_server <- function(id) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # create rv to hold early error messages

      preparse_check <- reactiveVal(
        data.frame(message = character(),
                   level = integer())
      )

      # parse data
      data <- reactive({
        req(input$upload)

        files <- input$upload

        valid_files <- grepl("\\.csv$", files$name, ignore.case = TRUE)

        if (!all(valid_files)) {
          preparse_check(
            data.frame(
              message = c("Only comma-separated (`.csv`) files can be uploaded"),
              level = c(3)
            )
          )
          return(NULL)
        } else {
          parse_raw_csv(files$datapath)
        }

      })

      # run checks
      checks <- reactive({
        if (isTruthy(data())) {
          # if data successfully parsed, run further checks
          upload_checks_raw(
            data(),
            vars = c(
              "class",
              "gender",
              "dobmnth", "dobday", "dobyr",
              "School ID code",
              "Local Authority",
              "postcode",
              "health",
              paste0("who", 1:5),
              paste0("lifesat", 1:5),
              paste0("fas", 1:5)
            )
          )
        } else {
          return(preparse_check())
        }

      })

      output$warn <- renderUI({
        req(checks(), cancelOutput = TRUE)

        warnings <- purrr::pmap(checks(), make_warning)

        do.call(tagList, warnings)
      })


      return(data)
    }
  )
}

# UI
# rawUploadUI('id')

# server
# rawUpload_server('id')
