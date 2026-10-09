if (!requireNamespace("testthat", quietly = TRUE)) stop("Install testthat first.")
for (path in c("single_proportion.R", "planning.R", "report.R", "reports_extended.R", "reports_objectives.R", "exports.R", "advanced.R", "effects.R", "plots.R", "reports_advanced.R", "advanced_ui.R", "app_server.R")) source(file.path("R", path))
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
