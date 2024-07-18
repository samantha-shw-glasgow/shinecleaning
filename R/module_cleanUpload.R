#' cleanUpload UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
cleanUploadUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Upload cleaned Excel file")
	)
}

#' cleanUpload Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
cleanUpload_server <- function(id){
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
# cleanUploadUI('id')

# server
# cleanUpload_server('id')
