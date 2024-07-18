render_report <- function(filename = "primary_report.docx") {

  rmarkdown::render("inst/templates/primary-reports/index.qmd",
                    output_dir = getwd(),
                    output_file = filename)

}
