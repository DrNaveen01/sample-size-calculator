# Normal-approximation planning for means and independent group comparisons.
# All probabilities are fractions. Calculations retain full numeric precision.

planning_confidence <- function(confidence, z = NULL) {
  if (is.null(z)) {
    check_number(confidence, "Confidence level", 0, 1, TRUE, TRUE)
    alpha <- 1 - confidence
    z <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    source <- "confidence"
  } else {
    check_number(z, "Z value", 0, Inf, TRUE)
    alpha <- 2 * stats::pnorm(z, lower.tail = FALSE)
    confidence <- 1 - alpha
    source <- "custom"
  }
  list(confidence = confidence, alpha = alpha, z = z, z_source = source)
}

check_planned_count <- function(x) {
  if (any(!is.finite(x)) || any(x <= 0) || any(x > 2^53 - 1)) {
    stop("The requested inputs produce a sample size outside the supported numeric range.", call. = FALSE)
  }
  invisible(x)
}

#' Plan precision for estimating one population mean
#' @param sd Anticipated population standard deviation in outcome units.
#' @param precision Absolute margin of error in the same units.
single_mean <- function(sd, precision, confidence = 0.95, nonresponse = 0, z = NULL) {
  check_number(sd, "Standard deviation", 0, Inf, TRUE)
  check_number(precision, "Margin of error", 0, Inf, TRUE)
  check_number(nonresponse, "Non-response proportion", 0, 1, FALSE, TRUE)
  critical <- planning_confidence(confidence, z)
  n0 <- (critical$z * sd / precision)^2
  adjusted <- n0 / (1 - nonresponse)
  check_planned_count(c(n0, adjusted))
  warnings <- if (ceiling(n0) < 30) {
    "The complete sample is below 30. When the standard deviation is estimated, a t-based calculation may require more observations."
  } else character()
  structure(c(list(method = "Normal approximation for one population mean", sd = sd,
    d = precision, precision = precision, nonresponse = nonresponse, n0 = n0,
    n_adjusted = adjusted, n_complete = ceiling(n0), n_final = ceiling(adjusted),
    warnings = warnings), critical), class = "single_mean_result")
}

comparison_settings <- function(alpha, power, ratio, n1, n2, nonresponse,
                                objective = "equality", direction = "higher",
                                margin = 0, lower = NULL, upper = NULL) {
  objective <- match.arg(objective, c("equality", "superiority", "noninferiority", "equivalence"))
  check_number(alpha, if (objective == "equality") "Two-sided significance level" else "One-sided significance level",
               0, if (objective == "equality") 1 else .5, TRUE, TRUE)
  if (objective %in% c("superiority", "noninferiority")) {
    direction <- match.arg(direction, c("higher", "lower"))
    check_number(margin, if (objective == "noninferiority") "Non-inferiority margin" else "Superiority margin",
                 0, Inf, objective == "noninferiority")
  } else {
    direction <- "higher"
    margin <- 0
  }
  if (objective == "equivalence") {
    check_number(lower, "Lower equivalence margin", -Inf, 0, FALSE, TRUE)
    check_number(upper, "Upper equivalence margin", 0, Inf, TRUE)
  } else lower <- upper <- NULL
  fixed <- !is.null(n1) || !is.null(n2)
  if (fixed) {
    for (i in 1:2) {
      n <- if (i == 1) n1 else n2
      check_number(n, paste0("Group ", i, " complete sample size"), 2, 2^53 - 1)
      if (n != floor(n)) stop("Group sizes must be whole numbers.", call. = FALSE)
    }
    check_planned_count(n1 + n2)
    ratio <- n2 / n1
    nonresponse <- 0
    power <- NULL
  } else {
    check_number(power, "Target power", 0.5, 1, TRUE, TRUE)
    check_number(ratio, "Allocation ratio (group 2 / group 1)", 0, Inf, TRUE)
    check_number(nonresponse, "Non-response proportion", 0, 1, FALSE, TRUE)
  }
  list(mode = if (fixed) "power" else "sample_size", objective = objective,
       direction = direction, direction_sign = if (direction == "higher") 1 else -1,
       margin = margin, lower = lower, upper = upper, alpha = alpha,
       z_alpha = stats::qnorm(if (objective == "equality") alpha / 2 else alpha, lower.tail = FALSE), power = power,
       beta = if (fixed) NULL else 1 - power,
       z_beta = if (fixed) NULL else stats::qnorm(power), ratio = ratio,
       nonresponse = nonresponse)
}

objective_name <- function(objective) switch(objective, equality = "Equality (two-sided difference test)",
  superiority = "Superiority", noninferiority = "Non-inferiority", equivalence = "Equivalence")

comparison_group_names <- function(group1, group2) {
  values <- lapply(list(group1, group2), function(value) {
    if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(trimws(value)) ||
        nchar(value) > 100 || grepl("[[:cntrl:]]", value)) {
      stop("Group names must contain 1 to 100 characters of single-line text.", call. = FALSE)
    }
    trimws(value)
  })
  if (identical(values[[1]], values[[2]])) stop("Use distinct names for the two groups.", call. = FALSE)
  list(group1 = values[[1]], group2 = values[[2]])
}

# Joint power of two one-sided normal tests. The two tests must both reject.
equivalence_power <- function(difference, se, lower, upper, z_alpha) {
  if (!is.finite(se) || se <= 0) stop("These inputs produce an unsupported standard error.", call. = FALSE)
  lo <- (lower - difference) / se + z_alpha
  hi <- (upper - difference) / se - z_alpha
  if (lo >= hi) return(0)
  # Choose a stable tail subtraction rather than subtracting two CDFs near one.
  if (lo > 0) stats::pnorm(lo, lower.tail = FALSE) - stats::pnorm(hi, lower.tail = FALSE)
  else stats::pnorm(hi) - stats::pnorm(lo)
}

equivalence_size <- function(variance, difference, settings) {
  if (difference <= settings$lower || difference >= settings$upper) {
    stop("The expected group 2 minus group 1 difference must lie strictly inside the equivalence margins for sample-size planning.", call. = FALSE)
  }
  check_number(variance, "Planning variance factor", 0, Inf, TRUE)
  power_at <- function(n) equivalence_power(difference, sqrt(variance / n), settings$lower, settings$upper, settings$z_alpha)
  bound <- 1
  while (power_at(bound) < settings$power) {
    bound <- bound * 2
    check_planned_count(c(bound, bound * settings$ratio))
  }
  stats::uniroot(function(n) power_at(n) - settings$power,
                 interval = c(.Machine$double.eps, bound), tol = 1e-9)$root
}

directional_gap <- function(difference, settings) {
  settings$direction_sign * difference - if (settings$objective == "noninferiority") -settings$margin else settings$margin
}

objective_power <- function(difference, se, settings) {
  if (!is.finite(se) || se <= 0) stop("These inputs produce an unsupported standard error.", call. = FALSE)
  if (settings$objective == "equivalence") return(equivalence_power(difference, se, settings$lower, settings$upper, settings$z_alpha))
  stats::pnorm(directional_gap(difference, settings) / se - settings$z_alpha)
}

objective_size <- function(variance, difference, settings) {
  if (settings$objective == "equivalence") return(equivalence_size(variance, difference, settings))
  gap <- directional_gap(difference, settings)
  if (!is.finite(gap) || gap <= 0) stop("The expected difference must satisfy the selected directional hypothesis and margin for sample-size planning.", call. = FALSE)
  (settings$z_alpha + settings$z_beta)^2 * variance / gap^2
}

# Within-group pooled SD, weighted by the reference study's degrees of freedom.
reference_pooled_sd <- function(sd1, sd2, ref_n1, ref_n2) {
  check_number(sd1, "Reference group 1 standard deviation", 0, Inf, TRUE)
  check_number(sd2, "Reference group 2 standard deviation", 0, Inf, TRUE)
  for (i in 1:2) {
    value <- if (i == 1) ref_n1 else ref_n2
    check_number(value, paste0("Reference group ", i, " sample size"), 2, 2^53 - 1)
    if (value != floor(value)) stop("Reference group sizes must be whole numbers.", call. = FALSE)
  }
  df1 <- ref_n1 - 1
  df2 <- ref_n2 - 1
  # Weights avoid multiplying a large reference count by a squared SD.
  sqrt(df1 / (df1 + df2) * sd1^2 + df2 / (df1 + df2) * sd2^2)
}

#' Taro Yamane simplified sample size for a finite population
yamane <- function(population, precision = .05, nonresponse = 0) {
  check_number(population, "Population size", 1, 2^53 - 1)
  if (population != floor(population)) stop("Population size must be a whole number.", call. = FALSE)
  check_number(precision, "Precision fraction", 0, 1, TRUE, TRUE)
  check_number(nonresponse, "Non-response proportion", 0, 1, FALSE, TRUE)
  n0 <- population / (1 + population * precision^2)
  adjusted <- n0 / (1 - nonresponse)
  check_planned_count(c(n0, adjusted))
  warnings <- if (ceiling(adjusted) > population) {
    "The required recruitment target exceeds the available population. The requested precision and non-response assumptions cannot be met by sampling this population."
  } else character()
  structure(list(method = "Taro Yamane simplified finite-population formula", population = population,
    d = precision, precision = precision, nonresponse = nonresponse, confidence = .95,
    n0 = n0, n_adjusted = adjusted, n_complete = ceiling(n0), n_final = ceiling(adjusted), warnings = warnings),
    class = "yamane_result")
}

comparison_counts <- function(n10, settings, n1, n2) {
  if (settings$mode == "power") {
    return(list(n10 = NULL, n20 = NULL, n1 = n1, n2 = n2,
      adjusted1 = NULL, adjusted2 = NULL, final1 = n1, final2 = n2,
      n_complete = n1 + n2, n_final = n1 + n2))
  }
  n20 <- settings$ratio * n10
  adjusted1 <- n10 / (1 - settings$nonresponse)
  adjusted2 <- n20 / (1 - settings$nonresponse)
  check_planned_count(c(n10, n20, adjusted1, adjusted2,
                       ceiling(adjusted1) + ceiling(adjusted2)))
  list(n10 = n10, n20 = n20, n1 = ceiling(n10), n2 = ceiling(n20),
       adjusted1 = adjusted1, adjusted2 = adjusted2,
       final1 = ceiling(adjusted1), final2 = ceiling(adjusted2),
       n_complete = ceiling(n10) + ceiling(n20),
       n_final = ceiling(adjusted1) + ceiling(adjusted2))
}

# Both rejection tails are included for the approximate fixed-size power.
comparison_power <- function(delta, se_null, se_alt, z_alpha) {
  if (!is.finite(se_null) || !is.finite(se_alt) || se_null <= 0 || se_alt <= 0) {
    stop("These inputs produce an unsupported standard error.", call. = FALSE)
  }
  stats::pnorm((delta - z_alpha * se_null) / se_alt) +
    stats::pnorm((-delta - z_alpha * se_null) / se_alt)
}

#' Two independent proportions, pooled null variance and unpooled alternative
#' @param ratio Planned complete sample size n2/n1 (not n1/n2).
#' @param n1,n2 Optional whole-number complete group sizes, to solve for power.
#' @param effect_type Direct p2, OR, or RR; OR and RR refer to group 2 / group 1.
two_proportions <- function(p1, p2 = NULL, power = 0.80, alpha = 0.05,
                            ratio = 1, nonresponse = 0, n1 = NULL, n2 = NULL,
                            effect_type = c("proportions", "or", "rr"), effect = NULL,
                            objective = "equality", direction = "higher", margin = 0,
                            lower = NULL, upper = NULL, group1 = "Group 1", group2 = "Group 2") {
  groups <- comparison_group_names(group1, group2)
  effect_type <- match.arg(effect_type)
  check_number(p1, "Group 1 proportion", 0, 1, TRUE, TRUE)
  if (effect_type != "proportions") {
    check_number(effect, if (effect_type == "or") "Odds ratio" else "Risk ratio", 0, Inf, TRUE)
    # Stable OR conversion, including large finite ratios.
    p2 <- if (effect_type == "or") 1 / (1 + ((1 - p1) / p1) / effect) else effect * p1
  }
  check_number(p2, "Group 2 proportion (entered or derived)", 0, 1, TRUE, TRUE)
  delta <- abs(p2 - p1)
  difference <- p2 - p1
  settings <- comparison_settings(alpha, power, ratio, n1, n2, nonresponse, objective, direction, margin, lower, upper)
  if (settings$objective %in% c("superiority", "noninferiority") && settings$margin >= 1) stop("A proportion margin must be below 1 (100 percentage points).", call. = FALSE)
  if (settings$objective == "equivalence" && (settings$lower <= -1 || settings$upper >= 1)) stop("Proportion equivalence margins must lie between -1 and 1.", call. = FALSE)
  if (delta == 0 && settings$objective == "equality" && settings$mode == "sample_size") stop("The two expected proportions must differ for sample-size planning.", call. = FALSE)
  k <- settings$ratio
  pbar <- (p1 + k * p2) / (1 + k)
  a <- sqrt((1 + 1 / k) * pbar * (1 - pbar))
  b <- sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k)
  n10 <- if (settings$mode == "sample_size") {
    if (settings$objective == "equality") ((settings$z_alpha * a + settings$z_beta * b) / delta)^2
    else objective_size(b^2, difference, settings)
  } else NULL
  counts <- comparison_counts(n10, settings, n1, n2)
  # Recompute the pooled proportion at the actual rounded group sizes.
  pooled_actual <- (counts$n1 * p1 + counts$n2 * p2) / counts$n_complete
  se_null <- sqrt(pooled_actual * (1 - pooled_actual) * (1 / counts$n1 + 1 / counts$n2))
  se_alt <- sqrt(p1 * (1 - p1) / counts$n1 + p2 * (1 - p2) / counts$n2)
  if (settings$objective != "equality") se_null <- se_alt
  achieved <- if (settings$objective == "equality") comparison_power(delta, se_null, se_alt, settings$z_alpha)
    else objective_power(difference, se_alt, settings)
  warnings <- character()
  if (any(c(counts$n1 * p1, counts$n1 * (1 - p1),
            counts$n2 * p2, counts$n2 * (1 - p2)) < 10)) {
    warnings <- "At least one group has fewer than 10 expected events or non-events. Normal-approximation power may be unreliable."
  }
  structure(c(list(method = paste(objective_name(settings$objective), "normal approximation for two independent proportions"),
    p1 = p1, p2 = p2, delta = delta, difference = difference, pbar = pbar, a = a, b = b,
    effect_type = effect_type, effect = if (effect_type == "proportions") NULL else effect,
    pooled_actual = pooled_actual, se_null = se_null, se_alt = se_alt,
    achieved_power = achieved, warnings = warnings), groups, settings, counts),
    class = c("two_proportions_result", "two_group_result"))
}

#' Two independent means, allowing different anticipated standard deviations
two_means <- function(mean1, mean2, sd1 = NULL, sd2 = sd1, power = 0.80,
                      alpha = 0.05, ratio = 1, nonresponse = 0,
                      n1 = NULL, n2 = NULL, objective = "equality", direction = "higher",
                      margin = 0, lower = NULL, upper = NULL,
                      sd_method = c("separate", "pooled", "reference"), pooled_sd = NULL,
                      ref_n1 = NULL, ref_n2 = NULL, group1 = "Group 1", group2 = "Group 2") {
  groups <- comparison_group_names(group1, group2)
  check_number(mean1, "Group 1 mean", -Inf, Inf)
  check_number(mean2, "Group 2 mean", -Inf, Inf)
  sd_method <- match.arg(sd_method)
  sd1_input <- sd1
  sd2_input <- sd2
  if (sd_method == "reference") pooled_sd <- reference_pooled_sd(sd1, sd2, ref_n1, ref_n2)
  if (sd_method != "separate") {
    check_number(pooled_sd, "Pooled standard deviation", 0, Inf, TRUE)
    sd1 <- sd2 <- pooled_sd
  } else {
    pooled_sd <- ref_n1 <- ref_n2 <- NULL
  }
  check_number(sd1, "Group 1 standard deviation", 0, Inf, TRUE)
  check_number(sd2, "Group 2 standard deviation", 0, Inf, TRUE)
  delta <- abs(mean2 - mean1)
  if (!is.finite(delta)) stop("The expected mean difference must be finite.", call. = FALSE)
  difference <- mean2 - mean1
  settings <- comparison_settings(alpha, power, ratio, n1, n2, nonresponse, objective, direction, margin, lower, upper)
  if (delta == 0 && settings$objective == "equality" && settings$mode == "sample_size") stop("The two expected means must differ for sample-size planning.", call. = FALSE)
  v <- sd1^2 + sd2^2 / settings$ratio
  n10 <- if (settings$mode == "sample_size") {
    if (settings$objective == "equality") ((settings$z_alpha + settings$z_beta) * sqrt(v) / delta)^2
    else objective_size(v, difference, settings)
  } else NULL
  counts <- comparison_counts(n10, settings, n1, n2)
  se <- sqrt(sd1^2 / counts$n1 + sd2^2 / counts$n2)
  achieved <- if (settings$objective == "equality") comparison_power(delta, se, se, settings$z_alpha)
    else objective_power(difference, se, settings)
  warnings <- if (min(counts$n1, counts$n2) < 30) {
    "At least one complete group size is below 30. A t-based design with estimated variances may require larger groups."
  } else character()
  structure(c(list(method = paste(objective_name(settings$objective), "normal approximation for two independent means"),
    mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2, delta = delta, difference = difference, variance_term = v,
    sd_method = sd_method, sd1_input = sd1_input, sd2_input = sd2_input,
    pooled_sd = pooled_sd, ref_n1 = ref_n1, ref_n2 = ref_n2,
    se_null = se, se_alt = se, achieved_power = achieved, warnings = warnings), groups, settings, counts),
    class = c("two_means_result", "two_group_result"))
}

print.single_mean_result <- function(x, ...) {
  cat("Single population mean\nComplete observations required: ", x$n_complete,
      "\nParticipants to approach: ", x$n_final, "\n", sep = "")
  invisible(x)
}

print.two_group_result <- function(x, ...) {
  cat(x$method, "\nComplete group sizes: ", x$group1, " = ", x$n1, "; ", x$group2, " = ", x$n2,
      if (x$mode == "sample_size") paste0("\nRecruitment group sizes: ", x$group1, " = ", x$final1, "; ", x$group2, " = ", x$final2) else "",
      "\nApproximate power at complete group sizes: ", format(round(100 * x$achieved_power, 2), scientific = FALSE, trim = TRUE), "%\n", sep = "")
  invisible(x)
}

print.yamane_result <- function(x, ...) {
  cat("Taro Yamane finite-population sample size\nComplete observations required: ", x$n_complete,
      "\nParticipants to approach: ", x$n_final, "\n", sep = "")
  invisible(x)
}
