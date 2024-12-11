#' createReport_groupings UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupingsUI <- function(id) {
  ns <- NS(id)

  tagList(shinyjs::hidden(
    textAreaInput(ns("groupings"), width = "100%", label = "Specify custom grouping. Grouped school-years should be on the same line.")
  ))
}

#' createReport_groupings Server
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReport_groupings_server <- function(id, custom_group, report_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    send_message <- make_send_message(session)

    # your code here


    observeEvent({
      custom_group()
      report_type()
    }, {
      if (isTRUE(custom_group())) {
        shinyjs::show("groupings")
      } else {
        updateTextAreaInput(session, "groupings", value = default())
        shinyjs::hide("groupings")
      }

    })


    default <- reactive({
      req(report_type())
      if (report_type() == "Primary" |
          report_type() == "Primary cluster / Local Authority") {
        default <- "P6\nP7"
      } else if (report_type() == "Secondary") {
        default <- "S1, S2\nS3, S4\nS5, S6"
      } else if (report_type() == "Secondary cluster / Local Authority") {
        default <- "S1\nS2\nS3\nS4\nS5\nS6"
      } else
        (default <- "")

      return(default)

    })

    group_list <- reactive({
      stringr::str_split_1(input$groupings, pattern = "\n") |>
        stringr::str_extract_all("[A-z][0-9]")
    })

    return(group_list)

  })
}

# UI
# createReport_groupingsUI('id')

# server
# createReport_groupings_server('id')
