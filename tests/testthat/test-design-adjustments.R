testthat::test_that("Design effect and finite population calculations have documented arithmetic", {
  original <- single_proportion(.5, .05, nonresponse = 0)
  direct <- single_proportion(.5, .05, nonresponse = 0, design_effect = 1.5)
  icc <- single_proportion(.5, .05, nonresponse = 0, icc = .025, cluster_size = 21)
  testthat::expect_identical(original$design_source, "none")
  testthat::expect_null(original$population_size)
  testthat::expect_false(grepl("Optional sampling adjustments", calculation_markdown(original), fixed = TRUE))
  testthat::expect_equal(direct$design_effect, 1.5)
  testthat::expect_match(calculation_markdown(direct), "Design effect")
  testthat::expect_false(grepl("### Finite population correction", calculation_markdown(direct), fixed = TRUE))
  testthat::expect_equal(icc$design_effect, 1.5)
  testthat::expect_equal(direct$n_srs, original$n0)
  testthat::expect_equal(direct$n0, original$n0 * 1.5)
  finite <- single_proportion(.5, .05, nonresponse = 0, population_size = 1000)
  testthat::expect_equal(finite$n0, original$n0 * 1000 / (999 + original$n0))
  testthat::expect_match(calculation_markdown(finite), "### Finite population correction")
  testthat::expect_false(grepl("### Design effect", calculation_markdown(finite), fixed = TRUE))
  testthat::expect_lte(finite$n_complete, 1000)
  mean <- single_mean(sd = 10, precision = 2, icc = .02, cluster_size = 26,
                      population_size = 500)
  testthat::expect_equal(mean$design_effect, 1.5)
  testthat::expect_equal(mean$n0, mean$n_design * 500 / (499 + mean$n_design))
  testthat::expect_error(single_proportion(.5, .05, design_effect = 2, icc = .1, cluster_size = 10))
  testthat::expect_error(single_mean(10, 2, icc = .1))
  testthat::expect_error(single_proportion(.5, .05, population_size = 17.5))
})

testthat::test_that("Two-group design effects multiply sizes and reduce power for fixed counts", {
  a <- two_proportions(.2, .3)
  b <- two_proportions(.2, .3, design_effect = 1.5)
  testthat::expect_equal(b$n10, a$n10 * 1.5)
  testthat::expect_equal(b$n10_srs, a$n10)
  fixed <- two_means(100, 105, pooled_sd = 15, sd_method = "pooled", n1 = 100, n2 = 100)
  inflated <- two_means(100, 105, pooled_sd = 15, sd_method = "pooled", n1 = 100, n2 = 100,
                        icc = .025, cluster_size = 21)
  testthat::expect_equal(inflated$design_effect, 1.5)
  testthat::expect_lt(inflated$achieved_power, fixed$achieved_power)
})

testthat::test_that("Direct signed mean difference preserves all SD modes", {
  a <- two_means(100, 105, sd_method = "pooled", pooled_sd = 15)
  b <- two_means(mean_difference = 5, sd_method = "pooled", pooled_sd = 15)
  testthat::expect_equal(a$n10, b$n10)
  testthat::expect_equal(b$mean_input, "difference")
  testthat::expect_match(calculation_markdown(b), "Entered signed")
  c <- two_means(mean_difference = 5, sd1 = 15, sd2 = 20,
                 sd_method = "reference", ref_n1 = 50, ref_n2 = 100)
  testthat::expect_gt(c$pooled_sd, 15)
  testthat::expect_error(two_means(100, 105, mean_difference = 5, sd_method = "pooled", pooled_sd = 15))
})

testthat::test_that("Single AUC default reports positive, negative and total planning targets", {
  x <- auc_sample(auc = .8, objective = "test", recruitment = "population", null_auc = .5, prevalence = .2, power = .8)
  testthat::expect_identical(x$objective, "test")
  testthat::expect_equal(x$required_diseased, x$strata$complete[1])
  testthat::expect_equal(x$required_nondiseased, x$strata$complete[2])
  testthat::expect_equal(x$n_complete, x$counts$complete[1])
  testthat::expect_gte(x$expected_diseased, x$required_diseased)
  testthat::expect_gte(x$expected_nondiseased, x$required_nondiseased)
  testthat::expect_match(calculation_markdown(x), "Minimum complete disease-positive")
  cc <- auc_sample(auc = .8, objective = "test", null_auc = .5,
    prevalence = .2, alpha = .05, power = .8,
    recruitment = "population", auc_method = "riskcalc")
  testthat::expect_match(cc$method, "RiskCalc")
  r <- .2 / .8
  v <- (1+r) * (.8/(r*(2-.8)) + 2*.8^2/(1+.8) - .8^2*(1/r+1)) / (.8^2*(1-.8)^2)
  raw <- ((stats::qnorm(.8) + stats::qnorm(.975)) / (stats::qlogis(.8)-stats::qlogis(.5)))^2 * v
  testthat::expect_equal(cc$required_diseased, ceiling(raw * .2))
  testthat::expect_equal(cc$required_nondiseased, ceiling(raw * .8))
  testthat::expect_equal(cc$n_complete, sum(ceiling(raw*c(.2,.8))))
})
