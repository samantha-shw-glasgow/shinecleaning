#' createReport UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReportUI <- function(id){
	ns <- NS(id)

	tagList(
	  h2("Report options"),
	  selectInput(ns('report_type'),
	              strong('Report type'),
	              choices = c('Primary', 'Secondary', 'Cluster / Local Authority', 'School-level data', 'Additional tables'),
	              selected = 'Primary'),
	  uiOutput(ns('report_ui')),
	  br(),
	  downloadButton(ns('generate'), 'Generate report'),

	  verbatimTextOutput(ns("test"))
	)
}

#' createReport Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from cleanUpload_server
#'
#' @keywords internal
createReport_server <- function(id, data){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# update UI for report type
				ui_options <- reactive({
				  if(input$report_type == 'Primary'){
				    tagList(
				      textInput(ns('school_name'), 'School name'),
				      numericInput(ns('n_invited'), 'Number of invited students', value = NA)
				    )
				  } else {
				    h4("coming soon...", class = 'text-center')
				  }
				})

				output$report_ui <- renderUI({
				  ui_options()
				})

				output$test <- renderPrint({
				  head(data()[,1:5])
				})

				output$generate <- downloadHandler(
				  filename = paste0(input$school_name, '_report.docx'),
				  content = function(file){
				    if(input$report_type == 'Primary'){

				      render_report(data(),
				                    school_name = input$school_name,
				                    filename = file,
				                    output_location = NULL)
				    }
				  }
				)
		}
	)
}

# UI
# createReportUI('id')

# server
# createReport_server('id')
