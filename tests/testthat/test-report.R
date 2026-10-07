testthat::test_that("reports include generic equations, legends, substitution and results", {
  text <- single_proportion_markdown(single_proportion(.5, .05, nonresponse = .1))
  for (phrase in c("Generic formulas", "Legends and input values", "Numerical substitution",
                   "384.14588", "426.82876", "427 participants", "\\frac", "References")) {
    testthat::expect_true(grepl(phrase, text, fixed = TRUE), info = phrase)
  }
  relative <- single_proportion_markdown(single_proportion(.2, .1, precision_type = "relative"))
  testthat::expect_true(grepl("d=\\epsilon p", relative, fixed = TRUE))
  testthat::expect_true(grepl("2 percentage points", relative, fixed = TRUE))
})

testthat::test_that("Markdown exports are the exact same content", {
  x <- single_proportion(.33, .05, nonresponse = .1)
  file <- tempfile(fileext = ".md")
  on.exit(unlink(file))
  single_proportion_export(x, file, "markdown")
  testthat::expect_identical(paste(readLines(file), collapse = "\n"),
                           sub("\n$", "", single_proportion_markdown(x)))
})

testthat::test_that("PDF and Word exports create valid files from the shared report", {
  testthat::skip_if_not(export_available("docx"))
  x <- single_proportion(.5, .05, nonresponse = .1)
  reference <- normalizePath(file.path("..", "..", "templates", "reference.docx"))
  word <- tempfile(fileext = ".docx")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(word, pdf)))
  single_proportion_export(x, word, "docx", reference_doc = reference)
  members <- utils::unzip(word, list = TRUE)$Name
  testthat::expect_true("word/document.xml" %in% members)
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  utils::unzip(word, files = "word/document.xml", exdir = folder)
  xml <- paste(readLines(file.path(folder, "word/document.xml"), warn = FALSE), collapse = "")
  testthat::expect_true(grepl("<m:oMath", xml, fixed = TRUE))
  testthat::expect_true(grepl("427 participants", xml, fixed = TRUE))
  if (export_available("pdf")) {
    single_proportion_export(x, pdf, "pdf")
    testthat::expect_identical(readChar(pdf, 4L, useBytes = TRUE), "%PDF")
  }
})
