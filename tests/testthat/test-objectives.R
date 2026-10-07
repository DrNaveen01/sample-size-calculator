testthat::test_that("one-sided mean hypotheses respect direction and margins", {
  # Independently checked with statsmodels normal_power_het, alternative='larger'.
  x <- two_means(100, 105, 15, objective = "superiority", alpha = .025, n1 = 100, n2 = 200)
  testthat::expect_equal(x$achieved_power, 0.7768778610779143, tolerance = 1e-11)
  reversed <- two_means(105, 100, 15, objective = "superiority", direction = "lower", alpha = .025, n1 = 200, n2 = 100)
  testthat::expect_equal(x$achieved_power, reversed$achieved_power)
  testthat::expect_equal(two_means(0, 0, 15, objective = "superiority", alpha = .025, n1 = 100, n2 = 100)$achieved_power, .025)
  adverse <- two_means(100, 95, 15, objective = "superiority", alpha = .025, n1 = 100, n2 = 200)
  testthat::expect_lt(adverse$achieved_power, .025)
  ni <- two_means(100, 100, 15, objective = "noninferiority", margin = 5, alpha = .025)
  superiority <- two_means(100, 105, 15, objective = "superiority", alpha = .025)
  testthat::expect_equal(ni$n10, superiority$n10)
  testthat::expect_equal(c(ni$n1, ni$n2), c(142, 142))
  testthat::expect_equal(two_means(100, 107, 15, objective = "superiority", margin = 2, alpha = .025)$n10, superiority$n10)
  testthat::expect_error(two_means(100, 95, 15, objective = "superiority"), "directional hypothesis")
  testthat::expect_error(two_means(100, 95, 15, objective = "noninferiority", margin = 5), "directional hypothesis")
  testthat::expect_error(two_means(100, 100, 15, objective = "noninferiority", margin = 0), "margin")
  testthat::expect_error(two_means(0, 1, 1e-200, objective = "superiority", n1 = 100, n2 = 100), "standard error")
})

testthat::test_that("equivalence uses joint power rather than independent test powers", {
  x <- two_means(0, 0, 10, objective = "equivalence", lower = -5, upper = 5, alpha = .05)
  # At D=0 and symmetric margins, solve joint power analytically.
  # V=200; n1=V*(z_.95+z_.90)^2 / 5^2 gives joint power .80.
  expected <- 200 * (stats::qnorm(.95) + stats::qnorm(.90))^2 / 25
  testthat::expect_equal(x$n10, expected, tolerance = 1e-10)
  testthat::expect_equal(c(x$n1, x$n2), c(69, 69))
  testthat::expect_gte(x$achieved_power, .8)
  testthat::expect_equal(equivalence_power(0, 10, -5, 5, stats::qnorm(.95)), 0)
  asymmetric <- two_means(0, 1, 10, 12, objective = "equivalence", lower = -3, upper = 5, ratio = 2)
  reversed <- two_means(1, 0, 12, 10, objective = "equivalence", lower = -5, upper = 3, ratio = .5)
  testthat::expect_equal(asymmetric$n10, reversed$n20, tolerance = 1e-10)
  testthat::expect_gte(asymmetric$achieved_power, .8)
  testthat::expect_error(two_means(0, 5, 10, objective = "equivalence", lower = -5, upper = 5), "strictly inside")
  outside <- two_means(0, 6, 10, objective = "equivalence", lower = -5, upper = 5, n1 = 500, n2 = 500)
  testthat::expect_lt(outside$achieved_power, .05)
  testthat::expect_error(two_means(0, 0, 10, objective = "equivalence", lower = 0, upper = 5), "Lower equivalence")
  testthat::expect_error(two_means(0, 0, 10, objective = "equivalence", lower = -5, upper = 0), "Upper equivalence")
})

testthat::test_that("binary objectives preserve signed differences and unpooled variances", {
  ni <- two_proportions(.6, .6, objective = "noninferiority", margin = .05, alpha = .025)
  testthat::expect_equal(c(ni$n1, ni$n2), c(1507, 1507))
  testthat::expect_gte(ni$achieved_power, .8)
  eq <- two_proportions(.2, .2, objective = "equivalence", lower = -.05, upper = .05)
  testthat::expect_equal(c(eq$n1, eq$n2), c(1097, 1097))
  testthat::expect_gte(eq$achieved_power, .8)
  for (objective in c("superiority", "noninferiority")) {
    x <- two_proportions(.6, .7, objective = objective, margin = if (objective == "noninferiority") .05 else 0, ratio = 2)
    reverse <- two_proportions(.7, .6, objective = objective, direction = "lower", margin = x$margin, ratio = .5)
    testthat::expect_equal(x$n10, reverse$n20, tolerance = 1e-10)
    testthat::expect_equal(x$se_null, x$se_alt)
    testthat::expect_gte(x$achieved_power, .8)
  }
  testthat::expect_error(two_proportions(.2, .3, objective = "noninferiority", margin = 1), "below 1")
  testthat::expect_error(two_proportions(.2, .2, objective = "equivalence", lower = -1, upper = .05), "between -1 and 1")
  testthat::expect_error(two_means(0, 1, 10, objective = "superiority", alpha = .5), "less than 0.5")
})

testthat::test_that("pooled SD uses reference degrees of freedom, not planned allocation", {
  expected <- sqrt((49 * 15^2 + 99 * 20^2) / 148)
  x <- two_means(100, 105, 15, 20, sd_method = "reference", ref_n1 = 50, ref_n2 = 100, ratio = 2)
  testthat::expect_equal(x$pooled_sd, expected)
  testthat::expect_equal(x$sd1, x$sd2)
  direct <- two_means(100, 105, sd_method = "pooled", pooled_sd = expected, ratio = 2)
  testthat::expect_equal(x$n10, direct$n10)
  testthat::expect_equal(reference_pooled_sd(10, 20, 5, 5), sqrt(250))
  testthat::expect_equal(reference_pooled_sd(20, 10, 9, 2), sqrt(3300/9))
  # Inactive separate SDs and reference counts do not invalidate direct pooled input.
  testthat::expect_s3_class(two_means(0, 1, NA, -1, sd_method = "pooled", pooled_sd = 5, ref_n1 = 1), "two_means_result")
  testthat::expect_error(two_means(0, 1, sd_method = "pooled", pooled_sd = 0), "Pooled standard")
  testthat::expect_error(reference_pooled_sd(10, 20, 1, 10), "at least 2")
  testthat::expect_error(reference_pooled_sd(10, 20, 10.5, 10), "whole numbers")
})

testthat::test_that("Yamane reproduces the finite-population example and assumptions", {
  x <- yamane(1000, .05, .1)
  testthat::expect_equal(x$n0, 1000 / 3.5)
  testthat::expect_equal(c(x$n_complete, x$n_final), c(286, 318))
  testthat::expect_equal(yamane(500, .05)$n_complete, 223)
  testthat::expect_equal(yamane(2000, .05)$n_complete, 334)
  # The IFAS table rounds to nearest whole observation; the app always rounds upward.
  testthat::expect_equal(yamane(1000, .05)$confidence, .95)
  testthat::expect_true(length(yamane(100, .001, .5)$warnings) > 0)
  testthat::expect_error(yamane(1000.5), "whole number")
  for (bad in c(0, -1, NA, Inf)) testthat::expect_error(yamane(bad))
  testthat::expect_error(yamane(1000, 0))
  testthat::expect_error(yamane(1000, .05, 1))
})

testthat::test_that("group names are validated and safely represented in reports", {
  x <- two_means(100, 105, 15, group1 = "Control", group2 = "New treatment")
  md <- calculation_markdown(x)
  testthat::expect_match(md, "Control")
  testthat::expect_match(md, "New treatment")
  testthat::expect_false(grepl("[Gg]roup [12]", md))
  unusual <- two_proportions(.2, .3, group1 = "Control | usual care", group2 = "Treatment <new> **arm**")
  html <- report_html_fragment(unusual)
  testthat::expect_false(grepl("<new>", html, fixed = TRUE))
  testthat::expect_match(html, "usual care")
  testthat::expect_error(two_means(0, 1, 10, group1 = ""), "Group names")
  testthat::expect_error(two_means(0, 1, 10, group2 = "Group 1"), "distinct")
})

testthat::test_that("all comparison objectives and Yamane provide complete numerical reports", {
  results <- list(yamane(1000, .05, .1),
    two_means(100,105,sd_method="pooled",pooled_sd=15),
    two_means(100,100,15,20,sd_method="reference",ref_n1=50,ref_n2=100,objective="noninferiority",margin=5,alpha=.025),
    two_means(0,1,10,12,objective="equivalence",lower=-3,upper=5,ratio=2),
    two_proportions(.2,.3,objective="superiority"),
    two_proportions(.2,.2,objective="noninferiority",margin=.05),
    two_proportions(.2,.21,objective="equivalence",lower=-.05,upper=.05),
    two_proportions(.2,effect_type="rr",effect=1.5,objective="superiority",n1=100,n2=200))
  for (x in results) {
    md <- calculation_markdown(x)
    for (heading in c("Generic formulas", "Legends and input values", "Numerical substitution", "Interpretation", "Assumptions and limitations", "References")) {
      testthat::expect_true(grepl(paste0("## ",heading),md,fixed=TRUE))
    }
    testthat::expect_false(grepl("ceil",md,fixed=TRUE))
    testthat::expect_true(grepl("n_{\\mathrm{final}}",md,fixed=TRUE) || identical(x$mode,"power"))
    if (inherits(x,"two_group_result")) testthat::expect_true(grepl(display_power(x$achieved_power),md,fixed=TRUE) || grepl(paste0(display_decimal(100*x$achieved_power),"\\%"),md,fixed=TRUE))
  }
  testthat::expect_match(calculation_markdown(results[[3]]),"Reference")
  testthat::expect_match(report_html_fragment(results[[3]]), "<h3[^>]*>Pooled standard deviation</h3>")
  testthat::expect_false(grepl("###", report_html_fragment(results[[3]]), fixed = TRUE))
  testthat::expect_match(calculation_markdown(results[[4]]),"solved numerically")
  testthat::expect_equal(display_power(.7768792841666379),"77.69%")
  testthat::expect_equal(display_power(.8),"80%")
  testthat::expect_equal(display_power(1),"100%")
})

testthat::test_that("new objectives match independent density-integration fixtures", {
  fixtures <- utils::read.csv(file.path("..", "fixtures", "independent-objective-planning.csv"))
  for (i in seq_len(nrow(fixtures))) {
    row <- fixtures[i, ]
    args <- list(objective=row$objective,alpha=row$alpha,ratio=row$ratio,margin=row$margin,
      lower=if (is.na(row$lower)) NULL else row$lower,upper=if (is.na(row$upper)) NULL else row$upper)
    x <- if (row$type=="two_means") do.call(two_means,c(list(mean1=row$mean1,mean2=row$mean2,sd1=row$sd1,sd2=row$sd2),args)) else
      do.call(two_proportions,c(list(p1=row$p1,p2=row$p2),args))
    testthat::expect_equal(x$n10,row$n10,tolerance=1e-10)
    testthat::expect_equal(c(x$n1,x$n2),c(row$n1,row$n2))
    testthat::expect_equal(x$achieved_power,row$achieved,tolerance=1e-10)
  }
})
