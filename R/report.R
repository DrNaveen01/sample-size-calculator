# One source of report content for the app, clipboard, and downloads.

display_number <- function(x, digits = 8L) {
  format(x, digits = digits, scientific = FALSE, trim = TRUE, big.mark = "")
}

display_count <- function(x) {
  format(x, scientific = FALSE, trim = TRUE, big.mark = ",", digits = 16)
}

display_percent <- function(x) paste0(display_number(100 * x, 6L), "%")

display_decimal <- function(x, places = 2L) {
  value <- formatC(x, format = "f", digits = places)
  if (places > 0) value <- sub("\\.?0+$", "", value)
  value
}

display_power <- function(x) paste0(display_decimal(100 * x), "%")

report_interpretation <- function(x) {
  precision_text <- if (x$precision_type == "relative") {
    paste0(display_percent(x$precision), " relative precision, equivalent to an absolute margin of ",
           display_number(100 * x$d, 6L), " percentage points")
  } else {
    paste0("an absolute margin of ", display_number(100 * x$d, 6L), " percentage points")
  }
  confidence_text <- if (x$z_source == "custom") {
    paste0("a specified Z value of ", display_number(x$z),
           " (implied two-sided confidence approximately ", display_percent(x$confidence), ")")
  } else {
    paste0("a two-sided confidence level of ", display_percent(x$confidence))
  }
  paste0("To estimate a population proportion of ", display_percent(x$p), " with ",
         precision_text, " and ", confidence_text, ", the normal-approximation calculation requires ",
         display_count(x$n_complete), " complete observations. Allowing for ",
         display_percent(x$nonresponse), " non-response, the final recruitment target is ",
         display_count(x$n_final), " participants. The non-response adjustment uses the unrounded base value, ",
         "and only the final recruitment target is rounded upward.")
}

single_proportion_markdown <- function(x) {
  stopifnot(inherits(x, "single_proportion_result"))
  v <- lapply(x[c("p", "d", "precision", "alpha", "z", "nonresponse", "n0", "n_adjusted")],
              display_number)
  formula <- function(text) c("", "$$", text, "$$", "")
  legends <- c(
    "| Symbol | Meaning | Value used |",
    "|:----|:---------------|----------:|",
    paste0("| $p$ | Expected population proportion | ", v$p, " (", display_percent(x$p), ") |"),
    paste0("| $d$ | Absolute margin of error on the proportion scale | ", v$d,
           " (", display_number(100 * x$d, 6L), " percentage points) |"),
    paste0("| $1-\\alpha$ | Two-sided confidence level | ", display_percent(x$confidence), " |"),
    paste0("| $\\alpha$ | Complement of the confidence level | ", v$alpha, " |"),
    paste0("| $Z_{1-\\alpha/2}$ | Standard-normal critical value | ", v$z, " |"),
    paste0("| $r$ | Expected non-response proportion | ", v$nonresponse,
           " (", display_percent(x$nonresponse), ") |"),
    paste0("| $n_0$ | Unrounded base number of complete observations | ", v$n0, " |"),
    paste0("| $n_{\\mathrm{adj}}$ | Unrounded number after non-response adjustment | ", v$n_adjusted, " |"),
    paste0("| $n_{\\mathrm{final}}$ | Final recruitment target rounded upward | ", display_number(x$n_final), " |")
  )
  if (x$precision_type == "relative") {
    legends <- c(legends, paste0("| $\\epsilon$ | Relative margin of error as a fraction of $p$ | ",
                                v$precision, " (", display_percent(x$precision), ") |"))
  }
  z_step <- if (x$z_source == "confidence") {
    c(
      formula("\\alpha = 1-C, \\qquad Z_{1-\\alpha/2}=\\Phi^{-1}(1-\\alpha/2)"),
      paste0("Here $C$ is the specified confidence level and $\\Phi^{-1}$ is the standard-normal quantile function."),
      formula(paste0("\\alpha = 1-", display_number(x$confidence), " = ", v$alpha)),
      formula(paste0("Z_{1-\\alpha/2}=\\Phi^{-1}(1-", v$alpha, "/2)\\approx ", v$z))
    )
  } else {
    c(
      "The user-specified Z value is used directly. Its implied two-sided confidence level is shown for context.",
      formula("C=2\\Phi(Z)-1, \\qquad \\alpha=1-C"),
      "Here $\\Phi$ is the standard-normal cumulative distribution function.",
      formula(paste0("Z = ", v$z, ", \\qquad C=2\\Phi(", v$z, ")-1\\approx ",
                     display_number(x$confidence)))
    )
  }
  precision_step <- if (x$precision_type == "relative") {
    c(
      "Relative precision is first converted to absolute precision.",
      formula("d=\\epsilon p"),
      formula(paste0("d=", v$precision, "\\times", v$p, "\\approx", v$d)),
      paste0("Thus ", display_percent(x$precision), " of ", display_percent(x$p),
             " is ", display_number(100 * x$d, 6L), " percentage points.")
    )
  } else {
    c(
      "Absolute precision is entered in percentage points and converted to a proportion.",
      formula("d=\\frac{d_{\\mathrm{pp}}}{100}"),
      "Here $d_{\\mathrm{pp}}$ is the margin of error in percentage points.",
      formula(paste0("d=\\frac{", display_number(100 * x$d, 6L), "}{100}\\approx", v$d))
    )
  }
  lines <- c(
    "# Sample size calculation for a single population proportion", "",
    paste0("**Final recruitment target: ", display_count(x$n_final), " participants.**"), "",
    "This report documents a precision-based sample-size calculation for estimating one population proportion.",
    "It uses the normal approximation with independent observations and optional non-response adjustment.", "",
    "## Generic formulas", "",
    formula("n_0 = \\frac{Z_{1-\\alpha/2}^{2}\\,p(1-p)}{d^{2}}"),
    formula("n_{\\mathrm{adj}} = \\frac{n_0}{1-r}"),
    "The complete sample $n_{\\mathrm{complete}}$ and final recruitment target $n_{\\mathrm{final}}$ are the respective unrounded values rounded upward to whole participants.", "",
    "## Legends and input values", "", legends, "",
    "## Numerical substitution", "",
    "### Confidence level and critical value", "", z_step, "",
    "### Precision", "", precision_step, "",
    "### Base sample size", "",
    formula(paste0("n_0\\approx\\frac{(", v$z, ")^{2}\\times", v$p,
                   "\\times(1-", v$p, ")}{(", v$d, ")^{2}}")),
    formula(paste0("n_0\\approx ", v$n0)),
    formula(paste0("n_{\\mathrm{complete}}=", display_number(x$n_complete))),
    "Here $n_{\\mathrm{complete}}$ is the whole-number target for complete observations before non-response adjustment.", "",
    "### Non-response adjustment and final rounding", "",
    formula(paste0("n_{\\mathrm{adj}}\\approx\\frac{", v$n0, "}{1-", v$nonresponse,
                   "}\\approx", v$n_adjusted)),
    formula(paste0("n_{\\mathrm{final}}=", display_number(x$n_final))),
    "Displayed decimals are shortened for readability. Calculations use full machine precision; displayed approximations are not fed back into the calculation.", "",
    "## Interpretation", "", report_interpretation(x), "",
    "## Assumptions and limitations", "",
    "- This method plans precision for estimation; it does not calculate power for a hypothesis test.",
    "- Sampling is assumed to yield independent observations representative of the target population.",
    "- The population is treated as large relative to the sample; no finite population correction is applied.",
    "- No cluster design effect or correction for measurement error is applied.",
    "- The normal approximation may perform poorly for rare outcomes or small expected category counts.",
    "- Non-response inflation compensates for expected loss of sample size; it does not remove non-response bias.",
    "- The anticipated proportion is a planning assumption, and the achieved precision depends on the observed data."
  )
  if (length(x$warnings)) lines <- c(lines, "", "### Checks for these inputs", "", paste0("- ", x$warnings))
  lines <- c(lines, "", "## References", "",
    "1. Lwanga SK, Lemeshow S. *Sample size determination in health studies a practical manual*. Geneva: World Health Organization; 1991. [WHO publication record](https://iris.who.int/handle/10665/40062).",
    "2. Penn State Department of Statistics. *STAT 500 Confidence intervals*. [Sample-size planning for one proportion](https://online.stat.psu.edu/stat500/Lesson05).", ""
  )
  paste(lines, collapse = "\n")
}
