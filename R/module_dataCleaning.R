#' dataCleaning UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
dataCleaningUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Cleaning options")
	)
}

#' dataCleaning Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from rawUpload_server
#'
#' @keywords internal
dataCleaning_server <- function(id, data){
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

				#return(clean_data) ## - must return reactive dataframe
		}
	)
}

# UI
# dataCleaningUI('id')

# server
# dataCleaning_server('id')
