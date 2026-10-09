# Run once from the project root in RStudio.
# Existing TeX Live / MiKTeX installations are reused.
packages <- c("shiny", "rmarkdown", "testthat", "base64enc", "renv")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

if (!rmarkdown::pandoc_available()) {
  stop("Pandoc was not found. Run this project in RStudio, or install Pandoc: https://pandoc.org/installing.html")
}
if (!nzchar(Sys.which("pdflatex"))) {
  if (identical(Sys.getenv("INSTALL_TINYTEX"), "true")) {
    if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex", repos = "https://cloud.r-project.org")
    tinytex::install_tinytex()
  } else {
    message("PDF export needs LaTeX. To install TinyTeX, run: Sys.setenv(INSTALL_TINYTEX = 'true'); source('scripts/setup.R')")
  }
}
message("Start the app with shiny::runApp('.')")

