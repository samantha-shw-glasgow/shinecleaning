#' cleaningOutput UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
cleaningOutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Download processed data"),
    downloadButton(ns("downloadData"), "Download")
  )
}

#' cleaningOutput Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from dataCleaning_server
#'
#' @keywords internal
cleaningOutput_server <- function(id, data) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          data() |>
            create_spreadsheet(file)
        }
      )
    }
  )
}

# UI
# cleaningOutputUI('id')

# server
# cleaningOutput_server('id')
