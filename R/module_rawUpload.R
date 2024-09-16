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
          "Download data for the required school(s) from Qualtrics in 'csv' format, then upload it here.",
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

      # upload data
      raw_data <- reactive({
        req(input$upload)
        readr::read_csv(input$upload$datapath,
          col_types = readr::cols(.default = "c"),
          show_col_types = F
        )[-1:-2, ]
      })

      # remove unwanted top rows, re-assign col types
      data <- reactive({
        df <- raw_data()
        drop <- NULL
        if (any(df[1, ] == colnames(df),
          na.rm = TRUE
        )) {
          drop <- c(drop, 1)
        }
        if (any(stringr::str_detect(df[2, ], "ImportId"),
          na.rm = TRUE
        )) {
          drop <- c(drop, 2)
        }
        if (length(drop) > 0) {
          df <- df[-drop, ]
        }

        df |> readr::type_convert() |> # is this a good idea?
          mutate(across(ends_with("Date"), as.character))
      })

      # run checks
      output$warn <- renderUI({
        req(data(), cancelOutput = TRUE)

        checks <- upload_checks_raw(
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

        warnings <- purrr::pmap(checks, make_upload_warning)

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
