testthat::test_that("Shiny state changes precision and clears invalid results", {
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(calculator_server, {
    session$setInputs(p_pct = 50, absolute_pct = 5, relative_pct = 10,
                      precision_type = "absolute", confidence_mode = "confidence",
                      confidence_pct = 95, nonresponse_pct = 10, z = 1.96)
    testthat::expect_equal(current_result()$n_final, 427)
    session$setInputs(precision_type = "relative")
    testthat::expect_equal(current_result()$n_final, 427)
    session$setInputs(p_pct = 20)
    testthat::expect_equal(current_result()$d, .02)
    testthat::expect_equal(current_result()$n_final, 1708)
    session$setInputs(p_pct = 0)
    testthat::expect_null(result_state()$result)
    testthat::expect_match(result_state()$error, "Expected proportion")
    session$setInputs(p_pct = 100)
    testthat::expect_match(result_state()$error, "less than 100", fixed = TRUE)
    session$setInputs(p_pct = 50, precision_type = "absolute", confidence_mode = "z", z = 2)
    testthat::expect_equal(current_result()$n_final, 445)
  })
})
