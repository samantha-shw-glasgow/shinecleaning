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
ui <- function(req) {
  navbarPage(
    theme = SHINE_theme,
    header = list(assets()),
    title = span(img(src = "img/SHINE_logo.png", height = 50)),
    windowTitle = "SHINE Mental Health Survey Tool",
    id = "main-menu",
    tabPanel(
      title = "Data Cleaning",
      layout_central_column(
        # shiny::h1("Data Cleaning"),
        # raw upload ui
        bslib::card(
          rawUploadUI("rawUpload")
        ),
        bslib::card(
          dataCleaningUI("dataCleaning")
        ),
        bslib::card(
          cleaningOutputUI("cleaningOutput")
        )
      ),
    ),
    bslib::nav_panel(
      title = "Report Generator",
      layout_central_column(
        # shiny::h1("Report Generator"),
        # raw upload ui
        bslib::card(
          cleanUploadUI("cleanUpload")
        ),
        bslib::card(
          createReportUI("createReport")
        )
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = NULL,
      value = "Help",
      icon = icon("question-circle"),
      layout_central_column(
        helpUI("help")
      )
    ),
  footer = tags$div(glue::glue("Dashboard version {packageVersion('SHINEcleaning')}"),
                    class = "footer")
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
assets <- function() {
  list(
    serveAssets(), # base assets (assets.R)
    tags$head(
      # Place any additional depdendencies here
      # e.g.: CDN,
      tags$link(rel = "shortcut icon", href = "img/shinemh.ico")
    ),
    shinyjs::useShinyjs()
  )
}
