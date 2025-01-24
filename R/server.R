#' Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @noRd
#' @keywords internal
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 10000 * 1024 ^ 2)

  send_message <- make_send_message(session)

  # data cleaning
  ## get uploaded raw data
  raw_data <- rawUpload_server("rawUpload") # this should return a reactive dataframe
  ## apply cleaninng rules
  cleaned_data <- dataCleaning_server("dataCleaning", data = raw_data) # this should return a reactive dataframe
  ## create and download excel sheet
  cleaningOutput_server("cleaningOutput", data = cleaned_data)


  # report gen
  ## get uploaded cleaned data
  report_data <- cleanUpload_server("cleanUpload") # this should return a reactive dataframe
  ## create and download report
  createReport_server("createReport", data = report_data)
}
