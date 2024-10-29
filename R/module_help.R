#' help UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
helpUI <- function(id) {
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
            bslib::card_body(
              h4("Detect duplicate cases"),
              "Flags responses that share the same date of birth, gender, and school ID as potential duplicates.",
              h4("Detect recurring postcodes"),
              paste(
                "Flags responses with a postcode that appears 6 or more times in the dataset.",
                "This may suggest that respondents have answered with their school postcode instead of their home postcode."
              ),
              h4("Detect partial cases"),
              "Flags responses where more than half of the questions are unanswered.",
              h4("Detect straightlining"),
              "Flags responses where the same answer has been given to every question within a group of questions.",
              h4("Detect age/year mismatch"),
              "Flags responses where the reported class differs by more than one year from what would be expected given the reported date of birth.",
              h4("Suggest class when missing"),
              "Flags responses where no class has been given, and makes a suggestion based on the respondent's date of birth where possible.",
              h4("Exclude test responses"),
              "Flags and marks for exclusion responses that are coded as test responses in Qualtrics.",
              h4("Exclude non-consenting"),
              "Flags and marks for exclusion responses that have not given consent to take part in the survey."
            )
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
help_server <- function(id) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
      ) {

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
