#' cleaningOutput UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
cleaningOutputUI <- function(id){
	ns <- NS(id)

	tagList(
		h2("Download processed data")
	)
}

#' cleaningOutput Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from dataCleaning_server
#'
#' @keywords internal
cleaningOutput_server <- function(id, data){
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
# cleaningOutputUI('id')

# server
# cleaningOutput_server('id')
