if (!requireNamespace("testthat", quietly = TRUE)) stop("Install testthat first.")
for (path in c("single_proportion.R", "report.R", "exports.R", "app_server.R")) source(file.path("R", path))
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
