#' createReport UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReportUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Report options"),
    selectInput(ns("report_type"),
      strong("Report type"),
      choices = c("Primary", "Secondary", "Primary cluster / Local Authority", "Secondary cluster / Local Authority", "School-level data", "Additional tables"),
      selected = "Primary"
    ),
    uiOutput(ns("report_warnings")),
    uiOutput(ns("report_ui")),
    uiOutput(ns("extra_warnings")),
    br(),
    shinyjs::disabled(downloadButton(ns("generate"), "Generate report"))
  )
}

#' createReport Server
#'
#' @param id Unique id for module instance.
#' @param data reactive dataframe, from cleanUpload_server
#'
#' @keywords internal
createReport_server <- function(id, data) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)


# Prep --------------------------------------------------------------------

      ## check if number of pupils is high enough for gender and class split options


      ## get all school IDs
      school_ids <- reactive({
        if (isTruthy(data())) {
          unique(data()$`School ID code`)
        } else {
          NA
        }
      })


# UI ----------------------------------------------------------------------


      ## update UI for report type
      ui_options <- reactive({
        req(data(), cancelOutput = T)
        # non-report outputs
        if (input$report_type %in% c("School-level data", "Additional tables")) {
          h4("coming soon...", class = "text-center")
        } else {
          # report generators
          tagList(
            # For school reports only
            if (input$report_type %in% c("Primary", "Secondary")) {
              tagList(
                selectInput(ns("school_id"), "School ID", choices = school_ids()),
                textInput(ns("name"), "School name")
              )
            },
            # For LA reports only
            if (input$report_type %in% c(
              "Primary cluster / Local Authority",
              "Secondary cluster / Local Authority"
            )) {
              tagList(
                selectizeInput(ns("school_id"), "School IDs",
                  multiple = T,
                  selected = "All",
                  choices = c("All", school_ids())
                ),
                textInput(ns("name"), "Local Authority / cluster name")
              )
            },
            # For all reports
            textInput(ns("school_term"), "Term of survey"),
            numericInput(ns("n_invited"),
                         "Number of invited students", value = NA),
            bslib::input_switch(ns("split"),
                                "Split by gender / school-year", value = T),
            createReport_groupingsUI(ns("grouping")),
            tableOutput(ns("preview"))
          )
        }
      })


      output$report_ui <- renderUI({
        ui_options()
      })


# pre checks ------------------------------------------------------------------


      ## check for required variables

      ## var names
      lifesat <- paste0("lifesat", 1:11)
      sch <- paste0("sch", 1:3)
      who <- paste0("who", 1:5)
      sehs <- paste0("sehs", 1:20)
      activity <- paste0("activity_", 4:14)

      primary_vars <- c(
        "gender",
        "health",
        lifesat,
        sch,
        who,
        sehs # , activity #
      )

      check_vars <- reactive({
        if (input$report_type == "Primary") {
          upcheck_has_columns(data(), primary_vars)
        }
      })

      output$report_warnings <- renderUI({
        if (isTRUE(check_vars()$fail)) {
          tagList(
            make_upload_warning(check_vars()$message, check_vars()$level)
          )
        }
      })


      ## disable download button if there are warnings using shinyjs
      observeEvent(check_vars(), ignoreNULL = F, {
        if (is.null(data())) {
          shinyjs::disable("generate")
        } else if (any(check_vars()$fail)) {
          shinyjs::disable("generate")
        } else {
          shinyjs::enable("generate")
        }
      })


# Filter data -------------------------------------------------------------


      ## filter by selected school IDs
      data_filt <- reactive({
        if (isTruthy(data())) {
          if (input$report_type %in% c("Primary", "Secondary")) {
            if (isTruthy(input$school_id) && !"All" %in% input$school_id) {
              data() %>% filter(`School ID code` == input$school_id)
            } else {
              data()
            }
          } else if (input$report_type %in% c(
            "Primary cluster / Local Authority",
            "Secondary cluster / Local Authority")) {
            if (isTruthy(input$school_id) && !"All" %in% input$school_id) {
              data() %>% filter(`School ID code` %in% input$school_id)
            } else {
              data()
            }
          } else if (input$report_type %in% c("School-level data", "Additional tables")) {
            data()
          }
        }
      })


# post checks -------------------------------------------------------------

    ## disable gender split switch if there are insufficient cases

      gender_split <- reactive({
        req(data_filt())
        if (isTRUE(input$report_type %in% c(
          "Primary cluster / Local Authority",
          "Secondary cluster / Local Authority",
          "Primary",
          "Secondary"
        ))) {
          if (nrow(data_filt()) < 10) {
            FALSE
          } else {
            TRUE
          }
        }
      })

      observeEvent({
        gender_split()
        input$report_type
      }, {

        if (isFALSE(gender_split())) {
          bslib::update_switch("split", value = FALSE)
          shinyjs::disable("split")
        }

        if (isTRUE(gender_split())) {
          shinyjs::enable("split")
          bslib::update_switch("split", value = TRUE)
        }

      })


      output$extra_warnings <- renderUI({
        req(data_filt(), cancelOutput = T)
        if (isFALSE(gender_split())) {
          tagList(
            make_upload_warning("Too few responses to split by gender & school-year", 1)
          )
        }
      })


# Group data ---------------------------------------------------------------

      class_list <- createReport_groupings_server("grouping",
                                                  data_filt,
                                                  report_type = reactive(input$report_type))

      grouped_data <- reactive({
        data_filt() |>
          mutate("classes_grouped" = group_classes(class, class_list()))
      })

      output$preview <- renderTable({
        if (input$report_type == "Primary" | input$report_type == "Primary cluster / Local Authority") inc_genders <- c("Girl", "Boy")
        else  inc_genders <- c("Girl", "Boy", "In another way")

        grouped_data() |>
          filter(gender %in% inc_genders,
                 class %in% unlist(class_list())) |>
          count(gender, `Year group` = classes_grouped) |>
          pivot_wider(names_from = gender, values_from = n)
      })

# Create report -----------------------------------------------------------


      ## create report

      output$generate <- downloadHandler(
        filename = function() {
          paste0(input$name, "_report.docx")
        },
        content = function(file) {
          showModal(modalDialog("Generating report...", footer = NULL))
          on.exit(removeModal(), add = TRUE)

          tryCatch(
            {
              if (input$report_type == "Primary") {
                cat(glue::glue("Rendering {input$report_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "primary",
                  school_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  #classes = class_list(),
                  output_location = NULL
                )
              }
              if (input$report_type == "Primary cluster / Local Authority") {
                cat(glue::glue("Rendering {input$report_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "primary",
                  local_authority_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  #classes = class_list(),
                  output_location = NULL
                )
              }
              if (input$report_type == "Secondary") {
                cat(glue::glue("Rendering {input$report_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "secondary",
                  school_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  classes = class_list(),
                  output_location = NULL
                )
              }
              if (input$report_type == "Secondary cluster / Local Authority") {
                cat(glue::glue("Rendering {input$report_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "secondary",
                  local_authority_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  classes = class_list(),
                  output_location = NULL
                )
              }
            },
            error = function(e) {
              showNotification(
                "Failed to generate report, please check the data.",
                type = "error"
              )
            }
          )
        }
      )
    }
  )
}

# UI
# createReportUI('id')

# server
# createReport_server('id')
