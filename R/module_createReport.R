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
	  uiOutput(ns('report_warnings')),
	  uiOutput(ns('report_ui')),
	  br(),
	  downloadButton(ns('generate'), 'Generate report'),
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


				## Prep
				## check if number of pupils is high enough for gender and class split options

				additional_options <- reactive({
				  if (!(input$report_type %in% c('School-level data', 'Additional tables'))) {
				    if (nrow(data()) < 10) {
				      FALSE
				    } else {
				      TRUE
				    }
				  }
				})

				## get all school IDs
				school_ids <- reactive({
				  if (isTruthy(data)) {
				    unique(data()$`School ID code`)
				  } else {
				    NA
				  }
				})


				## update UI for report type
				ui_options <- reactive({
				  if (input$report_type %in% c('Primary', 'Secondary')) {
				    tagList(
				      if (length(school_ids()) > 1) {
				        selectInput(ns('school_id'), 'School ID', choices = school_ids())
				      },
				      textInput(ns('school_name'), 'School name'),
				      textInput(ns('school_term'), 'Term of survey'),
				      numericInput(ns('n_invited'), 'Number of invited students', value = NA),
				      if (isTRUE(additional_options())) {
				        tagList(
				          checkboxInput(ns('split'), 'Split by gender and class', value = T)
				        )
				      },
				      if (isFALSE(additional_options())) {
				        make_upload_warning('Not enough respondents to split by class / gender', '1')
				      }
				    )
				  } else
				  if (input$report_type == 'Cluster / Local Authority') {
				    tagList(
				      selectizeInput(ns('school_id'), 'School IDs',
				                     multiple = T,
				                     choices = c("All" = NA, school_ids())),
				      textInput(ns('school_name'), 'Local Authority / cluster name'),
				      if (isTRUE(additional_options())) {
				        tagList(
				          checkboxInput(ns('gender_split'), 'Split by gender'),
				          checkboxInput(ns('class_split'), 'Split by class')
				        )
				      },
				      if (isFALSE(additional_options())) {
				        make_upload_warning('Not enough pupils to split by class / gender', '1')
				      }
				    )
				  } else
				  if (input$report_type %in% c('School-level data', 'Additional tables')) {
				    h4("coming soon...", class = 'text-center')
				  }
				},
				)


				output$report_ui <- renderUI({
				  ui_options()
				})

				## check for required variables

				## var names
				lifesat <- paste0('lifesat', 1:11)
				sch <- paste0('sch', 1:3)
				who <- paste0('who', 1:5)
				sehs <- paste0('sehs', 1:20)
				activity <- paste0('activity_', 4:14)

				primary_vars <- c("gender",
				                  "health",
				                  lifesat,
				                  sch,
				                  who,
				                  sehs#, activity #
				                  )

				check_vars <- reactive({
				  if (input$report_type == 'Primary') {
				    upcheck_has_columns(data(), primary_vars)
				  }
				})

				output$report_warnings <- renderUI({
				  if (isTRUE(check_vars()$fail)) {
				    tagList(
				      make_upload_warning(check_vars()$message, check_vars()$level)
				    )
				  }
				})

				## filter by selected school IDs
				data_filt <- reactive({
				  if (isTruthy(data())) {
				    if (input$report_type %in% c('Primary', 'Secondary')) {
				      if (isTruthy(input$school_id)) {
				        data() %>% filter(`School ID code` == input$school_id)
				      } else {
				        data()
				      }
				    } else
				    if (input$report_type == 'Cluster / Local Authority') {
				      if (!anyNA(input$school_id)) {
				        data() %>% filter(`School ID code` %in% input$school_id)
				      } else {
				        data()
				      }
				    } else
				    if (input$report_type %in% c('School-level data', 'Additional tables')) {
				      data()
				    }
				  }

				})


				## create report

				output$generate <- downloadHandler(
				  filename = function(){
				    paste0(input$school_name, '_report.docx')
				    },
				  content = function(file){
				    if(input$report_type == 'Primary'){

				      render_report(data_filt(),
				                    school_name = input$school_name,
				                    filename = file,
				                    number_invited = input$n_invited,
				                    gender_split = input$split,
				                    term = input$school_term,
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
