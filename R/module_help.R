#' help UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
helpUI <- function(id){
	ns <- NS(id)

	tagList(
		bslib::card(
		  h2("Help"),
		  bslib::card_body(
		    bslib::accordion(
		      id = ns("help"),
		      bslib::accordion_panel(
		        title = "Downloading data from Qualtrics",
		        h4("Step 1 - Use the 'Data & Analysis' tab to filter responses to date range"),
		        img(src = "img/qualtrics_1.jpeg", class = "help-img"),
		        img(src = "img/qualtrics_2.jpeg", class = "help-img"),
		        h4("Step 2 - Select 'Export Data...' from the 'Export & Import' menu"),
		        img(src = "img/qualtrics_3.jpeg", class = "help-img"),
		      ),
		      bslib::accordion_panel(
		        title = "Data cleaning rules",
		        "Placeholder text."
		      ),
		    )
		  )
		)
	)
}

#' help Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
help_server <- function(id){
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
		}
	)
}

# UI
# helpUI('id')

# server
# help_server('id')
