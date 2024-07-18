#' rawUpload UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUploadUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Upload data from Qualtrics"),
		fileInput(ns("upload"), "Select survey data", buttonLabel = "Upload..."),
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

				  warnings <- list()

				  if(nrow(data())>1){ #demo check 1
				    warnings$check1 <- tags$div(span(icon("triangle-exclamation"),
				                       "This is not the file you are looking for"),
				                       class="card p-2 bg-danger"
				                       )
				  }
				  if(ncol(data())>1){ #demo check 2
				    warnings$check2 <-
				                  tags$div(span(icon("circle-question"),
				                       "Are you sure this is right?"),
				                       class="card p-2 bg-warning"
				                       )
				  }

				  #combine into taglist
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
