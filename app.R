# Run this file with the RStudio Run App button, or shiny::runApp(".").
required <- c("shiny", "rmarkdown")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages using source('scripts/setup.R'): ", paste(missing, collapse = ", "), call. = FALSE)
if (!rmarkdown::pandoc_available()) stop("Pandoc is required. Use RStudio or install Pandoc before starting the app.", call. = FALSE)

for (file in c("single_proportion.R", "report.R", "exports.R", "app_ui.R", "app_server.R")) {
  source(file.path("R", file), local = TRUE)
}
shiny::shinyApp(ui = calculator_ui(), server = calculator_server)
