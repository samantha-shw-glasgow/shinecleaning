#' createReport_groupings UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupingsUI <- function(id){
	ns <- NS(id)

	tagList(
		bslib::input_switch(ns("custom_group"), label = "Use custom school-year groupings in report", value = F, width = "100%"),
	  textAreaInput(ns("groupings"),
	              width = "100%",
	              label = "Specify custom grouping. Grouped school-years should be on the same line."),
		tableOutput(ns("table"))
	)
}

#' createReport_groupings Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupings_server <- function(id, data, report_type){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# your code here

				group_list <- reactive({
				  str_split_1(input$groupings, pattern = "\n") |> str_extract_all("[A-z][0-9]")
				})

				default <- reactive({
				  if(report_type == "Primary" | report_type == "Primary cluster / Local Authority") "P6\nP7"
				  else if(report_type == "Secondary" | report_type == "Secondary cluster / Local Authority") "S1, S2, S3\nS4, S5, S6"
				})

				observe({
				  if (isTRUE(input$custom_group)) shinyjs::enable("groupings")
				  else {
				    updateTextAreaInput(session, "groupings",
				                        value = default())
				    shinyjs::disable("groupings")
				    }
				})

				grouped_data <- reactive({
				  data() |>
				    mutate("classes_grouped" = group_classes(class, group_list()))
				})

				output$table <- renderTable({
				  if (report_type == "Primary" | report_type == "Primary cluster / Local Authority") inc_genders <- c("Girl", "Boy")
				  else  inc_genders <- c("Girl", "Boy", "In another way")

				  grouped_data() |>
				    filter(gender %in% inc_genders,
				           class %in% unlist(group_list())) |>
				    count(gender, `Year group` = classes_grouped) |>
				    pivot_wider(names_from = gender, values_from = n)
				})
		}
	)
}

# UI
# createReport_groupingsUI('id')

# server
# createReport_groupings_server('id')
