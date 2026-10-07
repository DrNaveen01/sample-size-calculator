testthat::test_that("single mean precision retains units and full precision", {
  x <- single_mean(10, 2, nonresponse = .1)
  testthat::expect_equal(x$n0, 96.0364705173531, tolerance = 1e-12)
  testthat::expect_equal(x$n_complete, 97)
  testthat::expect_equal(x$n_final, 107)
  testthat::expect_equal(single_mean(30, 6)$n0, x$n0)
  testthat::expect_equal(single_mean(10, 1)$n0, 4 * x$n0)
  testthat::expect_equal(single_mean(10, 2, z = 2)$n0, 100)
  testthat::expect_equal(single_mean(10, 2, z = 2)$confidence, 0.954499736103642)
  testthat::expect_true(length(single_mean(1, 1)$warnings) > 0)
  for (bad in c(0, -1, NA, Inf)) {
    testthat::expect_error(single_mean(bad, 2))
    testthat::expect_error(single_mean(10, bad))
  }
})

testthat::test_that("two proportions reproduces the published PASS example", {
  # NCSS Tests for Two Proportions, example 2: .54 vs .44, 90%, alpha .05.
  x <- two_proportions(.54, .44, power = .9)
  testthat::expect_equal(c(x$n1, x$n2, x$n_complete), c(524, 524, 1048))
  testthat::expect_equal(x$achieved_power, .9005, tolerance = 1e-4)
  testthat::expect_equal(two_proportions(.2, .3, nonresponse = .1)$n_final, 652)
  higher <- two_proportions(.2, .3, power = .9)
  testthat::expect_gt(higher$n10, two_proportions(.2, .3)$n10)
})

testthat::test_that("unequal allocation and group reversal are consistent", {
  for (k in c(.5, 1, 2, 3)) {
    x <- two_proportions(.2, .3, ratio = k)
    reversed <- two_proportions(.3, .2, ratio = 1/k)
    testthat::expect_equal(x$n20, k * x$n10)
    testthat::expect_equal(x$n10, reversed$n20, tolerance = 1e-12)
    testthat::expect_equal(c(x$n1, x$n2), c(reversed$n2, reversed$n1))
    y <- two_means(10, 15, 10, 20, ratio = k)
    reversed_y <- two_means(15, 10, 20, 10, ratio = 1/k)
    testthat::expect_equal(y$n10, reversed_y$n20, tolerance = 1e-12)
    testthat::expect_equal(y$n20, k * y$n10)
  }
})

testthat::test_that("mean comparisons retain variance and rounding conventions", {
  x <- two_means(100, 105, 15, 15, nonresponse = .1)
  testthat::expect_equal(x$n10, 141.279835218283, tolerance = 1e-12)
  testthat::expect_equal(c(x$n1, x$n2, x$final1, x$final2), c(142, 142, 157, 157))
  # Inflating a rounded 142 would instead give 158: the app inflates unrounded n.
  testthat::expect_equal(x$n_final, 314)
  testthat::expect_equal(two_means(100, 110, 15, 15)$n10, x$n10 / 4)
  testthat::expect_equal(two_means(200, 210, 30, 30)$n10, x$n10)
  testthat::expect_gt(two_means(100, 105, 15, 20)$n10, x$n10)
  testthat::expect_true(length(two_means(0, 10, 1, 1)$warnings) > 0)
})

testthat::test_that("OR and RR use group 2 relative to group 1", {
  direct <- two_proportions(.2, .3)
  odds <- two_proportions(.2, effect_type = "or", effect = 12/7)
  risk <- two_proportions(.2, effect_type = "rr", effect = 1.5)
  testthat::expect_equal(odds$p2, .3)
  testthat::expect_equal(risk$p2, .3)
  testthat::expect_equal(odds$n10, direct$n10)
  testthat::expect_equal(risk$n10, direct$n10)
  testthat::expect_error(two_proportions(.8, effect_type = "rr", effect = 2), "Group 2 proportion")
  testthat::expect_error(two_proportions(.2, effect_type = "or", effect = 0), "Odds ratio")
})

testthat::test_that("fixed group sizes solve for both-tail approximate power", {
  x <- two_means(100, 105, 15, 15, n1 = 100, n2 = 200, nonresponse = .5)
  testthat::expect_equal(x$mode, "power")
  testthat::expect_equal(x$ratio, 2)
  testthat::expect_equal(x$nonresponse, 0)
  testthat::expect_equal(x$n_final, 300)
  testthat::expect_null(x$power)
  # Independently checked using statsmodels normal_power_het.
  testthat::expect_equal(x$achieved_power, 0.7768792841666379, tolerance = 1e-12)
  testthat::expect_gt(two_means(100, 105, 15, 15, n1 = 200, n2 = 400)$achieved_power, x$achieved_power)
  testthat::expect_equal(two_means(0, 0, 10, 10, n1 = 100, n2 = 200)$achieved_power, .05)
  testthat::expect_equal(two_proportions(.2, .2, n1 = 100, n2 = 200)$achieved_power, .05)
  a <- two_proportions(.2, .3, n1 = 100, n2 = 200)
  b <- two_proportions(.3, .2, n1 = 200, n2 = 100)
  testthat::expect_equal(a$achieved_power, b$achieved_power)
  testthat::expect_true(length(two_proportions(.001, .01, n1 = 10, n2 = 20)$warnings) > 0)
})

testthat::test_that("unequal group designs match independent statsmodels fixtures", {
  fixtures <- utils::read.csv(file.path("..", "fixtures", "independent-normal-planning.csv"))
  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    x <- if (row$type == "two_proportions") {
      two_proportions(row$p1, row$p2, power = row$power, alpha = row$alpha, ratio = row$ratio)
    } else two_means(row$mean1, row$mean2, row$sd1, row$sd2, power = row$power, alpha = row$alpha, ratio = row$ratio)
    testthat::expect_equal(x$n10, row$n10, tolerance = 1e-12)
    testthat::expect_equal(c(x$n1, x$n2), c(row$n1, row$n2))
    testthat::expect_equal(x$achieved_power, row$achieved, tolerance = 1e-12)
  }
})

testthat::test_that("comparison engines reject unusable planning inputs", {
  testthat::expect_error(two_proportions(.2, .2), "must differ")
  testthat::expect_error(two_means(0, 0, 10), "must differ")
  for (bad in c(0, -1, NA, Inf)) {
    testthat::expect_error(two_proportions(.2, .3, ratio = bad))
    testthat::expect_error(two_means(0, 1, bad))
  }
  for (bad in c(0, .5, 1, NA, Inf)) testthat::expect_error(two_proportions(.2, .3, power = bad))
  testthat::expect_error(two_proportions(.2, .3, n1 = 100))
  testthat::expect_error(two_proportions(.2, .3, n1 = 20.5, n2 = 40), "whole numbers")
  testthat::expect_error(two_means(0, 1, 10, n1 = 1, n2 = 2))
  testthat::expect_error(two_means(0, 1, 10, nonresponse = 1))
  testthat::expect_error(two_means(0, 1e-200, 10), "numeric range")
})
