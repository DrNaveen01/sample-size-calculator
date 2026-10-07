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

testthat::test_that("Shiny switches all calculators, allocation, and fixed-size power", {
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(calculator_server, {
    session$setInputs(calculator = "single_mean", mean_sd = 10, mean_precision = 2,
      confidence_mode = "confidence", confidence_pct = 95, z = 1.96, nonresponse_pct = 10,
      comparison_mode = "sample_size", alpha_pct = 5, power_pct = 80, allocation_ratio = 1,
      p1_pct = 20, p2_pct = 30, effect_type = "proportions", odds_ratio = 12/7, risk_ratio = 1.5,
      mean1 = 100, mean2 = 105, sd1 = 15, sd2 = 15, fixed_n1 = 100, fixed_n2 = 200)
    testthat::expect_equal(current_result()$n_final, 107)
    testthat::expect_match(markdown_report(), "single population mean")
    session$setInputs(calculator = "two_proportions")
    testthat::expect_equal(current_result()$n_final, 652)
    session$setInputs(allocation_ratio = 2)
    testthat::expect_equal(current_result()$n20, 2 * current_result()$n10)
    session$setInputs(power_pct = 90)
    testthat::expect_equal(current_result()$power, .9)
    session$setInputs(effect_type = "rr", risk_ratio = 1.5)
    testthat::expect_equal(current_result()$p2, .3)
    session$setInputs(risk_ratio = 6)
    testthat::expect_match(result_state()$error, "Group 2 proportion")
    session$setInputs(calculator = "two_means", power_pct = 80, allocation_ratio = 1)
    testthat::expect_equal(current_result()$n_final, 314)
    session$setInputs(comparison_mode = "power", nonresponse_pct = 100, power_pct = 100)
    # Inactive recruitment and target-power values do not invalidate power mode.
    testthat::expect_equal(current_result()$n_final, 300)
    testthat::expect_equal(current_result()$ratio, 2)
    testthat::expect_match(markdown_report(), "Power calculation")
    session$setInputs(fixed_n1 = 100.5)
    testthat::expect_match(result_state()$error, "whole numbers")
    session$setInputs(fixed_n1 = 100, calculator = "two_proportions", effect_type = "proportions")
    testthat::expect_equal(current_result()$mode, "power")
    session$setInputs(comparison_mode = "sample_size", nonresponse_pct = 10, power_pct = 50)
    testthat::expect_match(result_state()$error, "greater than 50")
    session$setInputs(power_pct = 80, p1_pct = 100)
    testthat::expect_match(result_state()$error, "less than 100")
  })
})

testthat::test_that("Shiny adds objectives, names, pooled SD and Yamane without inactive-input failures", {
  shiny::testServer(calculator_server, {
    session$setInputs(calculator="two_means",comparison_mode="sample_size",objective="noninferiority",
      direction="higher",mean1=100,mean2=100,sd_method="reference",sd1=15,sd2=20,ref_n1=50,ref_n2=100,
      ni_mean=5,alpha_pct=2.5,power_pct=80,allocation_ratio=2,nonresponse_pct=10,
      group1_name="Standard care",group2_name="New treatment",fixed_n1=100,fixed_n2=200)
    testthat::expect_equal(current_result()$objective,"noninferiority")
    testthat::expect_equal(current_result()$pooled_sd,sqrt((49*225+99*400)/148))
    testthat::expect_match(markdown_report(),"Standard care")
    testthat::expect_match(markdown_report(),"New treatment")
    session$setInputs(sd_method="pooled",pooled_sd=15,sd1=0,sd2=NA,ref_n1=1)
    testthat::expect_equal(current_result()$sd1,15)
    session$setInputs(objective="equivalence",lower_mean=-5,upper_mean=5,mean2=101,alpha_pct=5)
    testthat::expect_gte(current_result()$achieved_power,.8)
    session$setInputs(comparison_mode="power",fixed_n1=100,fixed_n2=200,power_pct=100,nonresponse_pct=100)
    testthat::expect_equal(current_result()$mode,"power")
    session$setInputs(group1_name="")
    testthat::expect_match(result_state()$error,"Group names")
    session$setInputs(calculator="yamane",population_size=1000,yamane_precision_pct=5,nonresponse_pct=10,
      confidence_mode="confidence",confidence_pct=100,group1_name="")
    testthat::expect_equal(current_result()$n_final,318)
    testthat::expect_match(markdown_report(),"Taro Yamane")
    session$setInputs(population_size=1000.5)
    testthat::expect_match(result_state()$error,"whole number")
  })
})
