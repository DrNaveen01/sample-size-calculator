# Shared Markdown for all calculators. Pandoc renders these equations in every output.
formula_lines <- function(text) c("", "$$", text, "$$", "")

calculation_key <- function(x) {
  if (inherits(x, "extended_result")) return(x$key)
  if (inherits(x, "single_proportion_result")) return("single_proportion")
  if (inherits(x, "single_mean_result")) return("single_mean")
  if (inherits(x, "two_proportions_result")) return("two_proportions")
  if (inherits(x, "two_means_result")) return("two_means")
  if (inherits(x, "yamane_result")) return("yamane")
  stop("Unsupported calculation result.", call. = FALSE)
}

calculation_markdown <- function(x) {
  text <- if (inherits(x, "extended_result")) extended_markdown(x) else switch(calculation_key(x), single_proportion = single_proportion_markdown(x),
    single_mean = single_mean_markdown(x), two_proportions = comparison_markdown(x),
    two_means = comparison_markdown(x), yamane = yamane_markdown(x))
  if (exists("plot_appendix", mode = "function")) text <- paste(c(text, plot_appendix(x)), collapse = "\n")
  text
}

legend_row <- function(symbol, meaning, value) {
  paste0("| $", symbol, "$ | ", meaning, " | ", value, " |")
}
legend_header <- function() c("| Symbol | Meaning | Value used |", "|:----|:---------------|----------:|")

confidence_substitution <- function(x) {
  f <- formula_lines
  d <- display_number
  if (x$z_source == "custom") {
    c("The supplied Z is used directly. $\\Phi$ is the standard-normal cumulative distribution function.",
      f("C=2\\Phi(Z)-1,\\qquad\\alpha=1-C"),
      f(paste0("Z=", d(x$z), ",\\qquad C=2\\Phi(", d(x$z), ")-1\\approx", d(x$confidence))),
      f(paste0("\\alpha=1-", d(x$confidence), "\\approx", d(x$alpha))))
  } else {
    c("$\\Phi^{-1}$ is the standard-normal quantile function and $C$ is the confidence level.",
      f("\\alpha=1-C,\\qquad Z_{1-\\alpha/2}=\\Phi^{-1}(1-\\alpha/2)"),
      f(paste0("\\alpha=1-", d(x$confidence), "=", d(x$alpha))),
      f(paste0("Z_{1-\\alpha/2}=\\Phi^{-1}(1-", d(x$alpha), "/2)\\approx", d(x$z))))
  }
}

single_mean_markdown <- function(x) {
  stopifnot(inherits(x, "single_mean_result"))
  f <- formula_lines
  d <- display_number
  lines <- c("# Sample size calculation for a single population mean", "",
    paste0("**Final recruitment target: ", display_count(x$n_final), " participants.**"), "",
    "This report plans estimation of one population mean with a specified absolute margin of error, using an anticipated standard deviation and a normal approximation.", "",
    "## Generic formulas", "",
    f("n_0=\\frac{Z_{1-\\alpha/2}^2\\sigma^2}{d^2}"),
    f("n_{\\mathrm{adj}}=\\frac{n_0}{1-r}"),
    "The complete sample $n_{\\mathrm{complete}}$ and final recruitment target $n_{\\mathrm{final}}$ are rounded upward from the respective unrounded values. The standard deviation and margin of error use the same measurement units.", "",
    "## Legends and input values", "", legend_header(),
    legend_row("\\sigma", "Anticipated population standard deviation", d(x$sd)),
    legend_row("d", "Absolute margin of error in outcome units", d(x$d)),
    legend_row("C=1-\\alpha", "Two-sided confidence level", display_percent(x$confidence)),
    legend_row("\\alpha", "Complement of confidence", d(x$alpha)),
    legend_row("Z_{1-\\alpha/2}", "Standard-normal critical value", d(x$z)),
    legend_row("r", "Expected non-response fraction", d(x$nonresponse)),
    legend_row("n_0", "Unrounded complete sample size", d(x$n0)),
    legend_row("n_{\\mathrm{adj}}", "Unrounded recruitment size", d(x$n_adjusted)),
    legend_row("n_{\\mathrm{complete}}", "Whole-number complete sample", d(x$n_complete)),
    legend_row("n_{\\mathrm{final}}", "Whole-number recruitment target", d(x$n_final)), "",
    "## Numerical substitution", "", "### Confidence and critical value", "", confidence_substitution(x),
    "### Complete sample size", "",
    f(paste0("n_0\\approx\\frac{(", d(x$z), ")^2\\times(", d(x$sd), ")^2}{(", d(x$d), ")^2}\\approx", d(x$n0))),
    f(paste0("n_{\\mathrm{complete}}=", d(x$n_complete))),
    "### Non-response and recruitment", "",
    f(paste0("n_{\\mathrm{adj}}\\approx\\frac{", d(x$n0), "}{1-", d(x$nonresponse), "}\\approx", d(x$n_adjusted))),
    f(paste0("n_{\\mathrm{final}}=", d(x$n_final))),
    "Displayed decimals are shortened; calculations retain full precision. Non-response inflation uses the unrounded base size.", "",
    "## Interpretation", "",
    paste0("With an anticipated standard deviation of ", d(x$sd), " and an absolute margin of error of ", d(x$d),
      " in the same units, ", display_percent(x$confidence), " two-sided confidence requires ", display_count(x$n_complete),
      " complete observations. Allowing for ", display_percent(x$nonresponse), " non-response gives a recruitment target of ", display_count(x$n_final), " participants."), "",
    "## Assumptions and limitations", "",
    "- This is precision planning for estimation; power is not a required input.",
    "- Observations are independent and the population is large relative to the sample.",
    "- The anticipated standard deviation is treated as known for planning. If it is estimated, a t-based design may require a larger sample.",
    "- Strong skewness and small samples may invalidate the normal approximation.",
    "- No finite population correction, cluster effect, or bias correction is applied.",
    "- Non-response inflation addresses expected loss of observations, not non-response bias.")
  if (length(x$warnings)) lines <- c(lines, "", "### Checks for these inputs", "", paste0("- ", x$warnings))
  lines <- c(lines, "", "## References", "",
    "1. Penn State Department of Statistics. *STAT 500 Confidence intervals*. [Planning precision for a population mean](https://online.stat.psu.edu/stat500/Lesson05).", "")
  paste(lines, collapse = "\n")
}

equality_markdown <- function(x) {
  stopifnot(inherits(x, "two_group_result"))
  f <- formula_lines
  d <- display_number
  proportions <- inherits(x, "two_proportions_result")
  sizing <- x$mode == "sample_size"
  sd_sections <- standard_deviation_sections(x)
  subject <- if (proportions) "two independent proportions" else "two independent means"
  title <- paste0("# ", if (sizing) "Sample size calculation for " else "Power calculation for ", subject)
  outcome_rows <- if (proportions) {
    c(legend_row("p_1", "Expected group 1 proportion", paste0(d(x$p1), " (", display_percent(x$p1), ")")),
      legend_row("p_2", "Expected group 2 proportion", paste0(d(x$p2), " (", display_percent(x$p2), ")")))
  } else {
    c(legend_row("\\mu_1", "Expected group 1 mean", d(x$mean1)),
      legend_row("\\mu_2", "Expected group 2 mean", d(x$mean2)),
      legend_row("\\sigma_1", "Group 1 standard deviation", d(x$sd1)),
      legend_row("\\sigma_2", "Group 2 standard deviation", d(x$sd2)))
  }
  effect_generic <- effect_numeric <- effect_row <- character()
  if (proportions && x$effect_type != "proportions") {
    if (x$effect_type == "or") {
      effect_row <- legend_row("OR", "Odds ratio for group 2 relative to group 1", d(x$effect))
      effect_generic <- f("p_2=\\frac{OR\\,p_1}{1-p_1+OR\\,p_1}")
      effect_numeric <- c("### Derive the second proportion", "",
        f(paste0("p_2=\\frac{", d(x$effect), "\\times", d(x$p1), "}{1-", d(x$p1), "+", d(x$effect), "\\times", d(x$p1), "}\\approx", d(x$p2))))
    } else {
      effect_row <- legend_row("RR", "Risk ratio for group 2 relative to group 1", d(x$effect))
      effect_generic <- f("p_2=RR\\,p_1")
      effect_numeric <- c("### Derive the second proportion", "",
        f(paste0("p_2=", d(x$effect), "\\times", d(x$p1), "\\approx", d(x$p2))))
    }
  }
  generic_size <- if (!sizing) character() else if (proportions) {
    c(f("\\delta=\\operatorname{abs}(p_2-p_1),\\qquad p_{\\mathrm{pool}}=\\frac{p_1+kp_2}{1+k}"),
      f("A=\\sqrt{(1+1/k)p_{\\mathrm{pool}}(1-p_{\\mathrm{pool}})}"),
      f("B=\\sqrt{p_1(1-p_1)+p_2(1-p_2)/k}"),
      f("n_{1,0}=\\frac{(Z_{1-\\alpha/2}A+Z_{1-\\beta}B)^2}{\\delta^2}"),
      "Here $A$ and $B$ are the null and alternative standard-error factors; $p_{\\mathrm{pool}}$ is the allocation-weighted planning proportion.")
  } else {
    c(f("\\delta=\\operatorname{abs}(\\mu_2-\\mu_1),\\qquad V=\\sigma_1^2+\\sigma_2^2/k"),
      f("n_{1,0}=\\frac{(Z_{1-\\alpha/2}+Z_{1-\\beta})^2V}{\\delta^2}"),
      "Here $V$ is the variance factor for the planned allocation.")
  }
  group_formulas <- if (sizing) {
    c(f("n_{2,0}=k n_{1,0}"),
      f("n_{i,\\mathrm{adj}}=\\frac{n_{i,0}}{1-r}"),
      f("n_{\\mathrm{final}}=n_{\\mathrm{final},1}+n_{\\mathrm{final},2}"),
      "For $i=1,2$, $n_{i,0}$ is the unrounded complete size, $n_i$ is the complete target rounded upward, $n_{i,\\mathrm{adj}}$ is the unrounded recruitment size, and $n_{\\mathrm{final},i}$ is the recruitment target rounded upward. The same non-response fraction is applied to both groups.")
  } else {
    c(f(if (proportions) "\\delta=\\operatorname{abs}(p_2-p_1)" else "\\delta=\\operatorname{abs}(\\mu_2-\\mu_1)"),
      "The supplied $n_1$ and $n_2$ are complete, analysable observations. No recruitment inflation is applied in power mode.")
  }
  power_generic <- if (proportions) {
    c(f("p_a=\\frac{n_1p_1+n_2p_2}{n_1+n_2}"),
      f("s_0=\\sqrt{p_a(1-p_a)(1/n_1+1/n_2)}"),
      f("s_1=\\sqrt{p_1(1-p_1)/n_1+p_2(1-p_2)/n_2}"),
      "Here $p_a$ is the expected pooled proportion weighted by the actual complete group sizes.")
  } else f("s_0=s_1=\\sqrt{\\sigma_1^2/n_1+\\sigma_2^2/n_2}")
  power_generic <- c(power_generic,
    f("u=\\frac{\\delta-Z_{1-\\alpha/2}s_0}{s_1},\\qquad v=\\frac{-\\delta-Z_{1-\\alpha/2}s_0}{s_1}"),
    f("P_{\\mathrm{approx}}=\\Phi(u)+\\Phi(v)"),
    "The operator $\\operatorname{abs}(x)$ is the absolute value. Here $s_0$ and $s_1$ are the null and alternative standard errors; $u$ and $v$ are standardized rejection-tail arguments. $\\Phi$ is the standard-normal cumulative distribution function. Both tails contribute to approximate power.")
  rows <- c(legend_header(), outcome_rows, effect_row,
    legend_row("\\delta", "Absolute difference to detect", d(x$delta)),
    legend_row("\\alpha", "Two-sided type I error probability", d(x$alpha)),
    legend_row("Z_{1-\\alpha/2}", "Significance critical value", d(x$z_alpha)),
    legend_row("k=n_2/n_1", if (sizing) "Planned group 2 / group 1 size ratio" else "Ratio of supplied group sizes", d(x$ratio)), sd_sections$rows)
  if (sizing) rows <- c(rows,
    legend_row("P=1-\\beta", "Target power", display_power(x$power)),
    legend_row("\\beta", "Type II error probability", d(x$beta)),
    legend_row("Z_{1-\\beta}", "Power quantile", d(x$z_beta)),
    legend_row("r", "Expected non-response fraction in each group", d(x$nonresponse)))
  rows <- c(rows, legend_row("n_1", "Complete observations in group 1", d(x$n1)),
    legend_row("n_2", "Complete observations in group 2", d(x$n2)),
    legend_row("P_{\\mathrm{approx}}", "Approximate power at these complete sizes", display_power(x$achieved_power)))
  critical_steps <- c(if (sizing) "### Significance and power quantiles" else "### Significance critical value", "",
    f("Z_{1-\\alpha/2}=\\Phi^{-1}(1-\\alpha/2)"),
    f(paste0("Z_{1-\\alpha/2}=\\Phi^{-1}(1-", d(x$alpha), "/2)\\approx", d(x$z_alpha))))
  if (sizing) critical_steps <- c(critical_steps,
    f("\\beta=1-P,\\qquad Z_{1-\\beta}=\\Phi^{-1}(P)"),
    f(paste0("\\beta=1-", d(x$power), "=", d(x$beta), ",\\qquad Z_{1-\\beta}=\\Phi^{-1}(", d(x$power), ")\\approx", d(x$z_beta))))
  delta_step <- f(if (proportions) paste0("\\delta=\\operatorname{abs}(", d(x$p2), "-", d(x$p1), ")\\approx", d(x$delta)) else
    paste0("\\delta=\\operatorname{abs}((", d(x$mean2), ")-(", d(x$mean1), "))\\approx", d(x$delta)))
  size_steps <- character()
  if (sizing) {
    size_steps <- c("### Difference and complete sample sizes", "", delta_step)
    if (proportions) {
      size_steps <- c(size_steps,
        f(paste0("p_{\\mathrm{pool}}=\\frac{", d(x$p1), "+", d(x$ratio), "\\times", d(x$p2), "}{1+", d(x$ratio), "}\\approx", d(x$pbar))),
        f(paste0("A\\approx\\sqrt{(1+1/", d(x$ratio), ")\\times", d(x$pbar), "\\times(1-", d(x$pbar), ")}\\approx", d(x$a))),
        f(paste0("B=\\sqrt{", d(x$p1), "(1-", d(x$p1), ")+", d(x$p2), "(1-", d(x$p2), ")/", d(x$ratio), "}\\approx", d(x$b))),
        f(paste0("n_{1,0}\\approx\\frac{(", d(x$z_alpha), "\\times", d(x$a), "+", d(x$z_beta), "\\times", d(x$b), ")^2}{(", d(x$delta), ")^2}\\approx", d(x$n10))))
    } else {
      size_steps <- c(size_steps,
        f(paste0("V=(", d(x$sd1), ")^2+(", d(x$sd2), ")^2/", d(x$ratio), "\\approx", d(x$variance_term))),
        f(paste0("n_{1,0}\\approx\\frac{(", d(x$z_alpha), "+", d(x$z_beta), ")^2\\times", d(x$variance_term), "}{(", d(x$delta), ")^2}\\approx", d(x$n10))))
    }
    size_steps <- c(size_steps,
      f(paste0("n_{2,0}\\approx", d(x$ratio), "\\times", d(x$n10), "\\approx", d(x$n20))),
      f(paste0("n_1=", d(x$n1), ",\\quad n_2=", d(x$n2))),
      "### Non-response and recruitment", "",
      f(paste0("n_{1,\\mathrm{adj}}\\approx\\frac{", d(x$n10), "}{1-", d(x$nonresponse), "}\\approx", d(x$adjusted1))),
      f(paste0("n_{2,\\mathrm{adj}}\\approx\\frac{", d(x$n20), "}{1-", d(x$nonresponse), "}\\approx", d(x$adjusted2))),
      f(paste0("n_{\\mathrm{final},1}=", d(x$final1), ",\\quad n_{\\mathrm{final},2}=", d(x$final2))),
      f(paste0("n_{\\mathrm{final}}=", d(x$final1), "+", d(x$final2), "=", d(x$n_final))))
  } else size_steps <- c("### Difference and supplied group sizes", "", delta_step,
    f(paste0("n_1=", d(x$n1), ",\\qquad n_2=", d(x$n2), ",\\qquad k=", d(x$n2), "/", d(x$n1), "\\approx", d(x$ratio))))
  power_steps <- c("### Approximate power at complete group sizes", "")
  if (proportions) {
    power_steps <- c(power_steps,
      f(paste0("p_a=\\frac{", d(x$n1), "\\times", d(x$p1), "+", d(x$n2), "\\times", d(x$p2), "}{", d(x$n1), "+", d(x$n2), "}\\approx", d(x$pooled_actual))),
      f(paste0("s_0\\approx\\sqrt{", d(x$pooled_actual), "(1-", d(x$pooled_actual), ")(1/", d(x$n1), "+1/", d(x$n2), ")}\\approx", d(x$se_null))),
      f(paste0("s_1=\\sqrt{", d(x$p1), "(1-", d(x$p1), ")/", d(x$n1), "+", d(x$p2), "(1-", d(x$p2), ")/", d(x$n2), "}\\approx", d(x$se_alt))))
  } else power_steps <- c(power_steps,
    f(paste0("s_0=s_1=\\sqrt{(", d(x$sd1), ")^2/", d(x$n1), "+(", d(x$sd2), ")^2/", d(x$n2), "}\\approx", d(x$se_alt))))
  u <- (x$delta - x$z_alpha * x$se_null) / x$se_alt
  v <- (-x$delta - x$z_alpha * x$se_null) / x$se_alt
  power_steps <- c(power_steps,
    f(paste0("u\\approx\\frac{", d(x$delta), "-", d(x$z_alpha), "\\times", d(x$se_null), "}{", d(x$se_alt), "}\\approx", d(u))),
    f(paste0("v\\approx\\frac{-", d(x$delta), "-", d(x$z_alpha), "\\times", d(x$se_null), "}{", d(x$se_alt), "}\\approx", d(v))),
    f(paste0("100P_{\\mathrm{approx}}\\approx100[\\Phi(", d(u), ")+\\Phi(", d(v), ")]\\approx", display_decimal(100 * x$achieved_power), "\\%")),
    paste0("Approximate power is ", display_power(x$achieved_power), ". Displayed power is rounded to at most two decimal places; all calculations retain full precision."))
  headline <- if (sizing) paste0("**Final recruitment target: ", display_count(x$n_final),
    " participants (group 1: ", display_count(x$final1), "; group 2: ", display_count(x$final2), ").**") else
    paste0("**Approximate power: ", display_power(x$achieved_power), " with group sizes ", display_count(x$n1), " and ", display_count(x$n2), ".**")
  interpretation <- if (sizing) paste0("To detect an absolute difference of ", d(x$delta),
    if (proportions) paste0(" (", d(100 * x$delta), " percentage points)") else " outcome units",
    " with target power ", display_power(x$power), " at a two-sided significance level of ", display_percent(x$alpha),
    ", the planned complete group sizes are ", display_count(x$n1), " and ", display_count(x$n2),
    ". Allowing for ", display_percent(x$nonresponse), " non-response in each group gives recruitment targets of ",
    display_count(x$final1), " and ", display_count(x$final2), " (total ", display_count(x$n_final),
    "). The planned group 2 / group 1 ratio is ", d(x$ratio), "; independent rounding can alter the exact ratio.") else
    paste0("With ", display_count(x$n1), " complete observations in group 1 and ", display_count(x$n2),
      " in group 2, approximate two-sided power to detect the specified difference is ", display_power(x$achieved_power),
      " at significance level ", display_percent(x$alpha), ". These are analysable group sizes; expected missing observations must be allowed for separately.")
  lines <- c(title, "", headline, "", paste0("This report compares ", subject,
    " using a two-sided test of equality and a normal approximation."), "",
    "## Generic formulas", "", f(if (proportions) "H_0:p_2-p_1=0,\\qquad H_1:p_2-p_1\\ne0" else "H_0:\\mu_2-\\mu_1=0,\\qquad H_1:\\mu_2-\\mu_1\\ne0"),
    sd_sections$generic, effect_generic, generic_size, group_formulas, power_generic, "",
    "## Legends and input values", "", rows, "",
    "## Numerical substitution", "", sd_sections$numeric, effect_numeric, critical_steps, size_steps, power_steps, "",
    "## Interpretation", "", interpretation, "", "## Assumptions and limitations", "",
    "- Groups and observations are independent. This method does not cover paired data or cluster designs.",
    "- This equality option tests a zero difference. A non-significant result does not demonstrate that the groups are identical or clinically equivalent.",
    if (proportions) "- Binary outcomes have the specified group probabilities. The null variance is pooled and the alternative variance is unpooled; no continuity correction or exact binomial calculation is applied." else
      "- Anticipated group standard deviations are treated as known for planning. Different standard deviations are allowed; this is a Z approximation, not an exact t-test or Welch power calculation.",
    if (sizing) "- The explicit sizing formula uses the rejection tail in the effect direction and neglects the far tail. The displayed approximate power check includes both tails at the rounded complete sizes." else
      "- Power is calculated from the entered planning effect and complete sample sizes. It is not post hoc evidence that an observed result is true.",
    "- Power and sample size depend on the planning effect and variance assumptions; departures can change performance.")
  if (sizing) lines <- c(lines, "- Non-response inflation uses unrounded sizes separately for each group. It compensates for expected loss of size and does not correct selection bias.")
  if (length(x$warnings)) lines <- c(lines, "", "### Checks for these inputs", "", paste0("- ", x$warnings))
  lines <- c(lines, "", "## References", "",
    if (proportions) c(
      "1. NCSS. *Tests for two proportions*, technical details on pooled and unpooled normal approximations. [PASS documentation](https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Two_Proportions.pdf).",
      "2. statsmodels. *Sample size for two independent proportions using the one relevant tail*. [Formula and allocation convention](https://www.statsmodels.org/stable/generated/statsmodels.stats.proportion.samplesize_proportions_2indep_onetail.html).") else c(
      "1. Penn State Department of Statistics. *STAT 507 Power and sample size considerations*. [Mean comparisons and unequal allocation](https://online.stat.psu.edu/stat507/Lesson10).",
      "2. statsmodels. *Normal power with specified null and alternative variances*. [Normal power documentation](https://www.statsmodels.org/stable/generated/statsmodels.stats.power.normal_power_het.html)."), "")
  if (!proportions && x$sd_method != "separate") lines <- c(lines,
    "3. Penn State Department of Statistics. *STAT 500 Comparing two population parameters*. [Within-group pooled variance](https://online.stat.psu.edu/stat500/Lesson07).", "")
  paste(lines, collapse = "\n")
}
