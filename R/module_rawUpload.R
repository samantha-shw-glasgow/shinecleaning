#' rawUpload UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
rawUploadUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Upload data from Qualtrics")
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

				# your code here

				#return(raw_data) ## - must return reactive dataframe
		}
	)
}

# UI
# rawUploadUI('id')

# server
# rawUpload_server('id')
