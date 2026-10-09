# Calculation engine. Inputs are proportions, not percentages.
# No formatting or intermediate rounding occurs in this file.

check_number <- function(x, name, lower, upper, lower_open = FALSE,
                         upper_open = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(name, " must be one finite number.", call. = FALSE)
  }
  if (if (lower_open) x <= lower else x < lower) {
    stop(name, " must be ", if (lower_open) "greater than " else "at least ",
         lower, ".", call. = FALSE)
  }
  if (if (upper_open) x >= upper else x > upper) {
    stop(name, " must be ", if (upper_open) "less than " else "at most ",
         upper, ".", call. = FALSE)
  }
  invisible(x)
}

#' Estimate sample size for one population proportion
#' @param p Expected proportion between zero and one.
#' @param precision Absolute precision, or a fraction of p for relative precision.
#' @param confidence Two-sided confidence level, e.g. 0.95.
#' @param nonresponse Expected fraction without an analysable response.
#' @param precision_type Either "absolute" or "relative".
#' @param z Optional positive critical value overriding confidence.
#' @return A structured single_proportion_result with full-precision values.
single_proportion <- function(p, precision, confidence = 0.95,
                             nonresponse = 0,
                             precision_type = c("absolute", "relative"),
                             z = NULL) {
  precision_type <- match.arg(precision_type)
  check_number(p, "Expected proportion", 0, 1, TRUE, TRUE)
  check_number(precision, "Precision", 0, 1, TRUE)
  check_number(nonresponse, "Non-response proportion", 0, 1, FALSE, TRUE)
  if (is.null(z)) {
    check_number(confidence, "Confidence level", 0, 1, TRUE, TRUE)
    alpha <- 1 - confidence
    z_value <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    z_source <- "confidence"
  } else {
    check_number(z, "Z value", 0, Inf, TRUE)
    z_value <- z
    alpha <- 2 * stats::pnorm(z_value, lower.tail = FALSE)
    confidence <- 1 - alpha
    z_source <- "custom"
  }
  d <- if (precision_type == "relative") p * precision else precision
  n0 <- (z_value * sqrt(p * (1 - p)) / d)^2
  adjusted <- n0 / (1 - nonresponse)
  if (!is.finite(adjusted) || adjusted <= 0 || adjusted > 2^53 - 1) {
    stop("The requested inputs produce a sample size outside the supported numeric range.",
         call. = FALSE)
  }
  base <- ceiling(n0)
  final <- ceiling(adjusted)
  warnings <- character()
  if (base * p < 10 || base * (1 - p) < 10) {
    warnings <- c(warnings, paste(
      "The planned complete sample has fewer than 10 expected observations",
      "in at least one outcome category. The normal approximation may be inadequate."
    ))
  }
  if (d >= min(p, 1 - p)) {
    warnings <- c(warnings, paste(
      "The planning interval reaches or crosses 0% or 100%.",
      "Interpret this normal-approximation calculation cautiously."
    ))
  }
  structure(list(
    method = "Normal approximation for one population proportion",
    p = p, precision = precision, precision_type = precision_type,
    d = d, confidence = confidence, alpha = alpha, z = z_value,
    z_source = z_source, nonresponse = nonresponse,
    n0 = n0, n_complete = base, n_adjusted = adjusted, n_final = final,
    warnings = warnings
  ), class = "single_proportion_result")
}

print.single_proportion_result <- function(x, ...) {
  cat("Single population proportion\n",
      "Complete observations required: ", x$n_complete, "\n",
      "Participants to approach: ", x$n_final, "\n", sep = "")
  invisible(x)
}

