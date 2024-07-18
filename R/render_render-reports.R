render_report <- function() {

  quarto::quarto_render("inst/templates/primary-reports/index.qmd", output_format = "docx")

}
