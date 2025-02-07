#' createReport UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
createReportUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Report options"),
    selectInput(ns("output_type"),
      strong("Report type"),
      choices = c("Primary", "Secondary",
                  "Primary cluster / Local Authority",
                  "Secondary cluster / Local Authority",
                  "Processed report data",
                  "School-level data"),
      selected = "Primary",
      width = "100%"
    ),
    uiOutput(ns("report_ui"))
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
        if (input$output_type  == "School-level data") {
          tagList(
            textInput(ns("name"), "File name", width = "100%"),
            selectInput(ns("data_output_type"), "Data type",
                        choices = c("Primary", "Secondary"),
                        width = "100%"),
            selectizeInput(ns("school_id"), "School IDs",
                           multiple = T,
                           selected = "All",
                           choices = c("All", school_ids()),
                           width = "100%"),
            bslib::input_switch(ns("custom_group"),
                                label = "Use custom school-year groupings",
                                value = F, width = "100%"),
            createReport_groupingsUI(ns("grouping")),
            tableOutput(ns("preview")),
            uiOutput(ns("additional_warnings")),
            downloadButton(ns("additional_output"), "Download data")
          )
        } else if (input$output_type == "Processed report data") {
          tagList(
            textInput(ns("name"), "File name", width = "100%"),
            selectInput(ns("data_output_type"), "Data type",
                        choices = c("Primary", "Secondary"),
                        width = "100%"),
            selectizeInput(ns("school_id"), "School IDs",
                             multiple = T,
                             selected = "All",
                             choices = c("All", school_ids()),
                             width = "100%"),
            make_warning(
              "This will return a spreadsheet of the processed data used to generate the reports.
              This should not be treated as an analysis-ready dataset.",
              2),
            uiOutput(ns("additional_warnings")),
            downloadButton(ns("additional_output"), "Download data")
          )
        } else {
          # report generators
          tagList(
            # For school reports only
            if (input$output_type %in% c("Primary", "Secondary")) {
              tagList(
                selectInput(ns("school_id"), "School ID", choices = school_ids(),
                            width = "100%"),
                textInput(ns("name"), "School name",
                          width = "100%")
              )
            },
            # For LA reports only
            if (input$output_type %in% c(
              "Primary cluster / Local Authority",
              "Secondary cluster / Local Authority"
            )) {
              tagList(
                selectizeInput(ns("school_id"), "School IDs",
                  multiple = T,
                  selected = "All",
                  choices = c("All", school_ids()),
                  width = "100%"
                ),
                textInput(ns("name"), "Local Authority / cluster name",
                          width = "100%")
              )
            },
            if (input$output_type == "Primary cluster / Local Authority") {
              textInput(ns("cluster_label"), "Group type (e.g. Local Authority,  Primary cluster, etc.)",
                        value = "Local Authority",
                        width = "100%")
            },
            # For all reports
            textInput(ns("school_term"), "Term of survey", width = "100%"),
            numericInput(ns("n_invited"),
                         "Number of invited students",
                         value = NA,
                         width = "100%"),
            bslib::input_switch(ns("split"),
                                "Split by gender / school-year",
                                value = T, width = "100%"),
            bslib::input_switch(ns("custom_group"),
                                label = "Use custom school-year groupings",
                                value = F, width = "100%"),
            createReport_groupingsUI(ns("grouping")),
            tableOutput(ns("preview")),
            uiOutput(ns("report_warnings")),
            shinyjs::disabled(downloadButton(ns("generate"), "Generate report"))
          )
        }
      })


      output$report_ui <- renderUI({
        ui_options()
      })


# Filter data -------------------------------------------------------------


      ## filter by selected school IDs
      data_filt <- reactive({
        if (isTruthy(data())) {
          if (input$output_type %in% c("Primary", "Secondary")) {
            if (isTruthy(input$school_id) && !"All" %in% input$school_id) {
              data() |> dplyr::filter(.data$`School ID code` == input$school_id)
            } else {
              data()
            }
          } else if (input$output_type %in% c(
            "Primary cluster / Local Authority",
            "Secondary cluster / Local Authority",
            "Processed report data",
            "School-level data")) {
            if (isTruthy(input$school_id) && !"All" %in% input$school_id) {
              data() |> dplyr::filter(.data$`School ID code` %in% input$school_id)
            } else {
              data()
            }
          }
        }
      })

# Group data ---------------------------------------------------------------

      class_list <- createReport_groupings_server("grouping",
                                                  custom_group = reactive(input$custom_group),
                                                  report_type = reactive({
                                                    if (input$output_type %in% c("Primary", "Primary cluster / Local Authority")) {
                                                      "Primary"
                                                    } else if (input$output_type %in% c("Secondary", "Secondary cluster / Local Authority")) {
                                                      "Secondary"
                                                    } else if (input$output_type %in% c("Processed report data", "School-level data")) {
                                                      input$data_output_type
                                                    }
                                                    })
                                                  )

      output$preview <- renderTable({
        data_filt() |>
          dplyr::mutate(`Year group` = factor(
            group_classes(class, class_list()),
            levels = unique(group_classes(unlist(class_list()), class_list()))
            ),
            gender = factor(.data$gender,
                            levels = c("Girl", "Boy", "In another way"))
            ) |>
          dplyr::filter(.data$gender %in% c("Girl", "Boy", "In another way"),
                 .data$class %in% unlist(class_list())) |>
          dplyr::count(.data$gender,
                .data$`Year group`,
                .drop = FALSE) |>
          tidyr::pivot_wider(names_from = "gender", values_from = "n")
      })




# Checks and warnings ------------------------------------------------------

      # Check if there are enough responses to produce reports

      enough_responses <- reactive({
        req(data_filt())
        if (nrow(data_filt()) >= 14) {
          TRUE
        } else {
          FALSE
        }
      })

      # Check if there are enough responses to split gender, class

      gender_split <- reactive({
        req(data_filt())

        nboys <- sum(data_filt()$gender == "Girl", na.rm = TRUE)
        ngirls <- sum(data_filt()$gender == "Boy", na.rm = TRUE)

        if (nboys >= 20 && ngirls >= 20) {
          TRUE
        } else {
          FALSE
        }

      })

      # check for required variables

      ## var names
      lifesat <- paste0("lifesat", 1:11)
      sch <- paste0("sch", 1:3)
      who <- paste0("who", 1:5)
      mm <- paste0("mm", 1:16)
      sehs_pri <- paste0("sehs", 1:20)
      sehs_sec <- paste0("sehss", 1:30)
      asw <- paste0("asw", 1:10)
      sdq <- paste0("sdq", 1:25)

      primary_vars <- c(
        "health",
        lifesat,
        sch,
        who,
        sehs_pri,
        mm
      )

      secondary_vars <- c(
        "health",
        lifesat,
        who,
        sehs_sec,
        sdq,
        asw
      )

      check_vars <- reactive({
        if (input$output_type %in% c("Primary", "Primary cluster / Local Authority")) {
          upcheck_has_columns(data(), primary_vars) |>
            dplyr::filter(.data$fail == TRUE) |>
            dplyr::select("message", "level")
        }

        if (input$output_type %in% c("Secondary", "Secondary cluster / Local Authority")) {
          upcheck_has_columns(data(), secondary_vars) |>
            dplyr::filter(.data$fail == TRUE) |>
            dplyr::select("message", "level")
        }

      })

      # check if all classes exist in the data, warn if not

      grouping_warnings <- reactive({
        allClasses <- unlist(class_list())

        if (length(allClasses) == 0) {
          return(
            data.frame(message = "No year-groups have been specified", level = 3)
          )
        } else if (any(duplicated(allClasses))) {
          return(
            data.frame(message = paste0(
              "The following year-groups have been specified more than once: ",
              paste(unique(allClasses[duplicated(allClasses)]), collapse = ", ")
            ), level = 3)
          )

        } else if (!all(allClasses %in% unique(data_filt()$class))) {

          missingClasses <- allClasses[!allClasses %in% unique(data_filt()$class)]

          return(
            data.frame(message = paste(
              "The following year-groups are expected but are missing from the data: ",
              paste(missingClasses, collapse = ", ")),
              level = 3)
          )
        }
      })

      # create warning boxes and disable download button if there are warnings

      all_warnings <- reactive({

        if (enough_responses() == FALSE) {
          return(data.frame(message = "There are not enough responses to generate a report", level = 3))
        } else {

          warnings <- rbind(check_vars(), grouping_warnings())

          if (gender_split() == FALSE && input$split == TRUE) {
            warnings <- rbind(warnings,
                              data.frame(message = "Splitting by gender / class is not recommended due to the low number of responses",
                                         level = 2))
          }

          return(warnings)
        }

      })

      output$report_warnings <- renderUI({

        if (any(all_warnings()$level == 3)) {
            shinyjs::disable("generate")
          } else {
            shinyjs::enable("generate")
          }

        tagList(
          purrr::pmap(
            all_warnings(),
            make_warning
          )
        )
      })

      ## alternate checks for additional outputs

      output$additional_warnings <- renderUI({

        req(input$data_output_type, cancelOutput = TRUE)

        error <- data.frame("message" = character(length = 0L),
                            "level" = numeric(length = 0L))

#        if (input$output_type %in% c("Processed report data", "School-level data")) {
        if (input$data_output_type == "Primary") {
          error <- upcheck_has_columns(data(), primary_vars) |>
            dplyr::filter(.data$fail == TRUE) |>
            dplyr::select("message", "level")
        }
        if (input$data_output_type == "Secondary") {
          error <- upcheck_has_columns(data(), secondary_vars) |>
            dplyr::filter(.data$fail == TRUE) |>
            dplyr::select("message", "level")
        }

        if (any(error$level == 3)) {
          shinyjs::disable("additional_output")
        } else {
          shinyjs::enable("additional_output")
        }

        tagList(
          purrr::pmap(
            error,
            make_warning
          )
        )

#        }


      })


# Create report -----------------------------------------------------------


      ## create report

      output$generate <- downloadHandler(
        filename = function() {
          name <- input$name
          if (name == "" | is.null(name)) {
            name <- "unnamed_SHINE"
          }
          paste0(name, "_report.docx")
        },
        content = function(file) {
          showModal(modalDialog("Generating report...", footer = NULL))
          on.exit(removeModal(), add = TRUE)

          tryCatch(
            {
              if (input$output_type == "Primary") {
                cat(glue::glue("Rendering {input$output_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "primary",
                  school_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  classes = class_list(),
                  output_location = NULL
                )
              }
              if (input$output_type == "Primary cluster / Local Authority") {
                cat(glue::glue("Rendering {input$output_type} report"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                render_report(data_filt(),
                  survey_type = "primary",
                  local_authority_name = input$name,
                  filename = file,
                  number_invited = input$n_invited,
                  gender_split = input$split,
                  term = input$school_term,
                  cluster_label = input$cluster_label,
                  classes = class_list(),
                  output_location = NULL
                )
              }
              if (input$output_type == "Secondary") {
                cat(glue::glue("Rendering {input$output_type} report"), "\n")
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
              if (input$output_type == "Secondary cluster / Local Authority") {
                cat(glue::glue("Rendering {input$output_type} report"), "\n")
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
              warning(paste0("Report error: ", e))
              showNotification(
                "Failed to generate report, please check the data.",
                type = "error"
              )
            }
          )
        }
      )

      # other outputs
      output$additional_output <- downloadHandler(
        filename = function() {
          name <- input$name
          if (name == "" | is.null(name)) {
            name <- "unnamed_SHINE_output"
          }
          paste0(name, ".xlsx")
        },
        content = function(file) {
          showModal(modalDialog("Generating output...", footer = NULL))
          on.exit(removeModal(), add = TRUE)

          tryCatch(
            {
              if (input$output_type == "Processed report data") {
                cat(glue::glue("Returning processed report data"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                report_data_spreadsheet(data_filt(), file, stringr::str_to_lower(input$data_output_type))
              }
              if (input$output_type == "School-level data") {
                cat(glue::glue("Returning school-level data"), "\n")
                cat(glue::glue("Data has {nrow(data_filt())} values"), "\n")
                report_derived_spreadsheet(data_filt(), file,
                                           stringr::str_to_lower(input$data_output_type),
                                           class_list())
              }
            },
            error = function(e) {
                warning(paste0("Output error: ", e))
                showNotification(
                  paste0("Failed to generate output. Error message: ", e),
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
