#' rawUpload UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUploadUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Upload data from Qualtrics"),
		fileInput(ns("upload"),
		          label = span("Select survey data",
		                       bslib::tooltip(
		                         icon("info-circle"),
		                         "Download data for the required school(s) from Qualtrics in 'csv' format, then upload it here.",
		                         placement = "right")
		          ),
		          buttonLabel = "Upload...",
		          accept = ".csv"),
		uiOutput(ns("warn"))
	)
}

#' rawUpload Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUpload_server <- function(id){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# upload data
				data <- reactive({
				  req(input$upload)
				  read.csv(input$upload$datapath)
				})

				# run checks
				output$warn <- renderUI({

				  req(data(), cancelOutput = TRUE)

				  checks <- upload_checks(data())
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
