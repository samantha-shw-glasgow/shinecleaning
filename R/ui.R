#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @import shiny
#' @importFrom bslib bs_theme
#'
#' @keywords internal
ui <- function(req){
	navbarPage(
		theme = bs_theme(version = 5),
		header = list(assets()),
		title = "SHINE Mental Health Survey Reporting Tool",
		id = "main-menu",
		tabPanel(
			"Data Cleaning",
			shiny::h1("Data Cleaning"),
			# raw upload ui
			bslib::card(
			  rawUploadUI('rawUpload')
			  ),
			bslib::card(
			  dataCleaningUI('dataCleaning')
			),
			bslib::card(
			  cleaningOutputUI('cleaningOutput')
			)
		),
		tabPanel(
			"Report Generator",
			shiny::h1("Report Generator"),
			# raw upload ui
			bslib::card(
			  cleanUploadUI('cleanUpload')
			),
			bslib::card(
			  createReportUI('createReport')
			)
		)
	)
}

#' Assets
#'
#' Includes all assets.
#' This is a convenience function that wraps
#' [serveAssets] and allows easily adding additional
#' remote dependencies (e.g.: CDN) should there be any.
#'
#' @importFrom shiny tags
#'
#' @keywords internal
assets <- function(){
	list(
		serveAssets(), # base assets (assets.R)
		tags$head(
			# Place any additional depdendencies here
			# e.g.: CDN
		)
	)
}
