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
		      open = FALSE,
		      id = ns("help"),
		      bslib::accordion_panel(
		        title = "Downloading data from Qualtrics",
		        bslib::card_body(
		          h4("Step 1 - Use the 'Data & Analysis' tab to filter responses by 'School ID code' for the chosen school(s)"),
		          img(src = "img/qualtrics_1.jpeg", class = "help-img"),
		          img(src = "img/qualtrics_2.jpeg", class = "help-img"),
		          h4("Step 2 - Select 'Export Data...' from the 'Export & Import' menu"),
		          img(src = "img/qualtrics_3.jpeg", class = "help-img"),
		          h4("Step 3 - Choose 'CSV' as the file format, ensure the settings under 'More options' match the images below, and click 'Download'"),
		          img(src = "img/qualtrics_4.jpeg", class = "help-img"),
		          img(src = "img/qualtrics_5.jpeg", class = "help-img")
		          )
		        ),
		      bslib::accordion_panel(
		        title = "Data cleaning rules",
		        "Coming soon."
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
