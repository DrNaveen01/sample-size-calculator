testthat::test_that("independently tabulated normal quantiles give expected counts", {
  cases <- data.frame(
    p = c(.50, .20, .33, .10, .50), d = c(.05, .05, .05, .03, .05),
    confidence = c(.95, .95, .95, .95, .99),
    raw = c(384.145882069412, 245.853364524423, 339.738618102188,
            384.145882069412, 663.489660102122),
    final = c(385, 246, 340, 385, 664)
  )
  for (i in seq_len(nrow(cases))) {
    x <- single_proportion(cases$p[i], cases$d[i], cases$confidence[i])
    testthat::expect_equal(x$n0, cases$raw[i], tolerance = 1e-10)
    testthat::expect_equal(x$n_final, cases$final[i])
  }
})

testthat::test_that("non-response adjustment uses the unrounded base", {
  x <- single_proportion(.50, .05, .95, .10)
  testthat::expect_equal(x$n_complete, 385)
  testthat::expect_equal(x$n_adjusted, 426.828757854902, tolerance = 1e-10)
  testthat::expect_equal(x$n_final, 427)
  testthat::expect_false(x$n_final == ceiling(x$n_complete / .90))
})

testthat::test_that("relative and absolute precision agree after conversion", {
  absolute <- single_proportion(.20, .02, .95, .10)
  relative <- single_proportion(.20, .10, .95, .10, precision_type = "relative")
  testthat::expect_equal(absolute$n0, relative$n0)
  testthat::expect_equal(relative$d, .02)
  testthat::expect_equal(absolute$n_final, relative$n_final)
})

testthat::test_that("an explicit Z value is authoritative", {
  x <- single_proportion(.50, .05, confidence = .90, z = 2, nonresponse = .10)
  testthat::expect_equal(x$n0, 400)
  testthat::expect_equal(x$n_final, 445)
  testthat::expect_equal(x$confidence, .954499736103642, tolerance = 1e-12)
  testthat::expect_equal(x$z_source, "custom")
})

testthat::test_that("invalid and extreme inputs are rejected", {
  for (p in list(0, 1, -.1, 1.1, NA_real_, Inf, NaN, c(.2, .3), "0.5", TRUE)) {
    testthat::expect_error(single_proportion(p, .05), "Expected proportion")
  }
  for (d in list(0, -.1, 1.1, NA_real_, Inf, numeric())) {
    testthat::expect_error(single_proportion(.5, d), "Precision")
  }
  for (r in c(-.1, 1, 2, NA_real_)) testthat::expect_error(single_proportion(.5, .05, nonresponse = r), "Non-response")
  for (c in c(0, 1, -.1, NA_real_)) testthat::expect_error(single_proportion(.5, .05, confidence = c), "Confidence")
  testthat::expect_error(single_proportion(.5, .05, z = 0), "Z value")
  testthat::expect_error(single_proportion(.5, .05, z = Inf), "finite")
  testthat::expect_error(single_proportion(.5, 1e-200), "numeric range")
})

testthat::test_that("precision and prevalence have the expected planning relationships", {
  x <- single_proportion(.2, .04)
  testthat::expect_equal(single_proportion(.2, .02)$n0, 4 * x$n0)
  testthat::expect_equal(single_proportion(.8, .04)$n0, x$n0)
  testthat::expect_gt(single_proportion(.5, .04)$n0, x$n0)
  testthat::expect_length(single_proportion(.5, .05)$warnings, 0)
  testthat::expect_true(length(single_proportion(.001, .02)$warnings) > 0)
})
