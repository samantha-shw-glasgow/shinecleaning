#' createReport_groupings UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupingsUI <- function(id, defaults = c("Primary", "Secondary")){
	ns <- NS(id)

	tagList(
		bslib::input_switch(ns("custom_group"), label = "Use custom school-year groupings in report", value = F, width = "100%"),
		shinyjs::disabled(
		  textAreaInput(ns("groupings"),
		              width = "100%",
		              label = "Specify custom grouping. Grouped school-years should be on the same line.",
		              value = ifelse(defaults == "Primary",
		                             "P6\nP7",
		                             "S1, S2, S3\nS4, S5, S6"))
		  ),
		#htmlOutput(ns("group_text"))
		tableOutput(ns("table"))
	)
}

#' createReport_groupings Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupings_server <- function(id, data){
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

				output$test <- renderPrint(group_list())

				# output$group_text <- renderUI({
				#   group_list() |>
				#     imap(~paste0("Group ", .y,": ", paste0(.x, collapse= ", "))) |>
				#     str_flatten("<br/>") |> HTML()
				# })

				observeEvent(input$custom_group, ignoreInit = T, {
				  shinyjs::toggleState("groupings")
				})

				grouped_data <- reactive({
				  data() |>
				    mutate("classes_grouped" = group_classes(class, group_list()))
				})

				output$table <- renderTable({
				  grouped_data() |>
				    filter(gender %in% c("Girl", "Boy", "In another way"),
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
