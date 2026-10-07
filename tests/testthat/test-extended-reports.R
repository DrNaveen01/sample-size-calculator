testthat::test_that("every calculator and power mode has a complete report", {
  results <- list(single_mean(10, 2, nonresponse = .1),
    two_proportions(.2, .3, ratio = 2, nonresponse = .1),
    two_means(100, 105, 15, 20, ratio = 2, nonresponse = .1),
    two_proportions(.2, effect_type = "or", effect = 12/7),
    two_proportions(.2, effect_type = "rr", effect = 1.5),
    two_proportions(.2, .3, n1 = 100, n2 = 200),
    two_means(-10, -5, 12, 8, n1 = 100, n2 = 200))
  for (x in results) {
    md <- calculation_markdown(x)
    for (heading in c("Generic formulas", "Legends and input values", "Numerical substitution", "Interpretation", "Assumptions and limitations", "References")) {
      testthat::expect_true(grepl(paste0("## ", heading), md, fixed = TRUE))
    }
    testthat::expect_true(grepl("$$", md, fixed = TRUE))
    destination <- tempfile(fileext = ".md")
    calculation_export(x, destination, "markdown")
    testthat::expect_identical(readChar(destination, file.info(destination)$size, useBytes = TRUE), md)
    if (inherits(x, "two_group_result")) {
      testthat::expect_true(grepl(display_number(x$n1), md, fixed = TRUE))
      testthat::expect_true(grepl(display_number(x$n2), md, fixed = TRUE))
      testthat::expect_true(grepl(display_power(x$achieved_power), md, fixed = TRUE))
    }
  }
  testthat::expect_match(calculation_markdown(results[[4]]), "Odds ratio")
  testthat::expect_match(calculation_markdown(results[[5]]), "Risk ratio")
  testthat::expect_match(calculation_markdown(results[[6]]), "No recruitment inflation")
  testthat::expect_true(grepl("abs}((105)-(100))", calculation_markdown(two_means(100,105,15)), fixed=TRUE))
})

testthat::test_that("new report types produce native Word math and PDF output", {
  testthat::skip_if_not(export_available("docx"))
  for (x in list(single_mean(10,2), two_proportions(.2,.3,ratio=2), two_means(100,105,15,20),
                 two_means(100,105,15,20,n1=100,n2=200))) {
    word <- tempfile(fileext=".docx")
    calculation_export(x, word, "docx", reference_doc = file.path("..", "..", "templates", "reference.docx"))
    directory <- tempfile()
    dir.create(directory)
    utils::unzip(word, files="word/document.xml", exdir=directory)
    xml <- paste(readLines(file.path(directory,"word/document.xml"),warn=FALSE),collapse="")
    testthat::expect_true(grepl("<m:oMath",xml,fixed=TRUE))
    testthat::expect_true(grepl("Numerical substitution",xml,fixed=TRUE))
    unlink(directory,recursive=TRUE)
    if (export_available("pdf")) {
      pdf <- tempfile(fileext=".pdf")
      calculation_export(x,pdf,"pdf")
      testthat::expect_identical(readChar(pdf,4,useBytes=TRUE),"%PDF")
    }
  }
})
