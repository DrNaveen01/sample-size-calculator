# Objective-specific reports use the same Markdown source for every export.

standard_deviation_sections <- function(x) {
  empty <- list(generic = character(), numeric = character(), rows = character())
  if (!inherits(x, "two_means_result") || x$sd_method == "separate") return(empty)
  f <- formula_lines
  d <- display_number
  generic <- c("", "### Pooled standard deviation", "", f("\\sigma_1=\\sigma_2=s_p"),
    "The pooled SD $s_p$ is a common within-group standard deviation for planning. It assumes equal population variances.")
  numeric <- c("### Pooled standard deviation", "")
  rows <- legend_row("s_p", if (x$sd_method == "reference") "Computed within-group pooled SD" else "Directly reported pooled SD", d(x$pooled_sd))
  if (x$sd_method == "reference") {
    generic <- c(generic, f("s_p=\\sqrt{\\frac{(m_1-1)s_{R1}^2+(m_2-1)s_{R2}^2}{m_1+m_2-2}}"),
      "Here $m_1,m_2$ are the reference study group sizes and $s_{R1},s_{R2}$ are its within-group SDs. Reference sizes weight the SD; they do not set this study's allocation or sample size.")
    rows <- c(rows, legend_row("s_{R1}", "Reference group 1 SD", d(x$sd1_input)),
      legend_row("s_{R2}", "Reference group 2 SD", d(x$sd2_input)),
      legend_row("m_1", "Reference group 1 sample size", d(x$ref_n1)),
      legend_row("m_2", "Reference group 2 sample size", d(x$ref_n2)))
    numeric <- c(numeric, f(paste0("s_p=\\sqrt{\\frac{(", d(x$ref_n1), "-1)(", d(x$sd1_input), ")^2+(",
      d(x$ref_n2), "-1)(", d(x$sd2_input), ")^2}{", d(x$ref_n1), "+", d(x$ref_n2), "-2}}\\approx", d(x$pooled_sd))))
  } else numeric <- c(numeric, "The pooled SD is entered directly from the reference study; it is not recomputed from group sizes.")
  numeric <- c(numeric, f(paste0("\\sigma_1=\\sigma_2=s_p=", d(x$pooled_sd))))
  list(generic = generic, numeric = numeric, rows = rows)
}

escape_report_text <- function(value) {
  chars <- strsplit(value, "", fixed = TRUE)[[1]]
  special <- c("\\", "`", "*", "_", "{", "}", "[", "]", "<", ">", "#", "|", "$")
  paste0(ifelse(chars %in% special, paste0("\\", chars), chars), collapse = "")
}

name_report_groups <- function(markdown, x) {
  matches <- gregexpr("[Gg]roup [12]", markdown)
  found <- regmatches(markdown, matches)[[1]]
  if (length(found)) regmatches(markdown, matches) <- list(vapply(found, function(value) {
    escape_report_text(if (endsWith(value, "1")) x$group1 else x$group2)
  }, character(1)))
  markdown
}

comparison_markdown <- function(x) {
  stopifnot(inherits(x, "two_group_result"))
  md <- if (x$objective == "equality") equality_markdown(x) else objective_markdown(x)
  name_report_groups(md, x)
}

objective_markdown <- function(x) {
  f <- formula_lines
  d <- display_number
  proportions <- inherits(x, "two_proportions_result")
  sizing <- x$mode == "sample_size"
  equivalence <- x$objective == "equivalence"
  subject <- if (proportions) "two independent proportions" else "two independent means"
  sd <- standard_deviation_sections(x)
  variance <- if (proportions) x$b^2 else x$variance_term
  difference_formula <- if (proportions) "D=p_2-p_1" else "D=\\mu_2-\\mu_1"
  difference_numeric <- if (proportions) paste0("D=", d(x$p2), "-", d(x$p1), "\\approx", d(x$difference)) else
    paste0("D=(", d(x$mean2), ")-(", d(x$mean1), ")\\approx", d(x$difference))
  variance_formula <- if (proportions) "V=p_1(1-p_1)+p_2(1-p_2)/k" else "V=\\sigma_1^2+\\sigma_2^2/k"
  variance_numeric <- if (proportions) paste0("V=", d(x$p1), "(1-", d(x$p1), ")+", d(x$p2), "(1-", d(x$p2), ")/", d(x$ratio), "\\approx", d(variance)) else
    paste0("V=(", d(x$sd1), ")^2+(", d(x$sd2), ")^2/", d(x$ratio), "\\approx", d(variance))
  rows <- c(legend_header(), if (proportions) c(
    legend_row("p_1", "Expected group 1 proportion", d(x$p1)), legend_row("p_2", "Expected group 2 proportion", d(x$p2))) else c(
    legend_row("\\mu_1", "Expected group 1 mean", d(x$mean1)), legend_row("\\mu_2", "Expected group 2 mean", d(x$mean2)),
    legend_row("\\sigma_1", "Group 1 planning SD", d(x$sd1)), legend_row("\\sigma_2", "Group 2 planning SD", d(x$sd2))), sd$rows,
    legend_row("D", "Signed expected difference: group 2 minus group 1", d(x$difference)),
    legend_row("k=n_2/n_1", if (sizing) "Planned group 2 / group 1 allocation ratio" else "Ratio of supplied complete sizes", d(x$ratio)),
    legend_row("\\alpha", if (equivalence) "Significance level for each one-sided test" else "One-sided significance level", d(x$alpha)),
    legend_row("Z_{1-\\alpha}", "One-sided standard-normal critical value", d(x$z_alpha)),
    legend_row("V", "Variance factor for planned allocation", d(variance)))
  if (sizing) rows <- c(rows, legend_row("P_\\star", "Target power", display_power(x$power)),
    legend_row("\\beta=1-P_\\star", "Type II error probability", d(x$beta)),
    if (!equivalence) legend_row("Z_{1-\\beta}", "Power quantile", d(x$z_beta)),
    legend_row("r", "Expected non-response fraction in each group", d(x$nonresponse)))
  rows <- c(rows, legend_row("n_1", "Complete group 1 observations", d(x$n1)),
    legend_row("n_2", "Complete group 2 observations", d(x$n2)),
    legend_row("s", "Standard error at complete sizes", d(x$se_alt)),
    legend_row("P_{\\mathrm{approx}}", "Approximate power at complete sizes", display_power(x$achieved_power)))
  if (equivalence) {
    hypotheses <- f("H_0:D\\le L\\;\\mathrm{or}\\;D\\ge U,\\qquad H_1:L<D<U")
    rows <- c(rows, legend_row("L", "Lower equivalence margin", d(x$lower)), legend_row("U", "Upper equivalence margin", d(x$upper)))
    hypothesis_text <- "Equivalence requires rejection of both one-sided null hypotheses. Margins refer to the signed group 2 minus group 1 difference."
    hypothesis_numeric <- c(f(paste0("H_0:D\\le", d(x$lower), "\\;\\mathrm{or}\\;D\\ge", d(x$upper))),
      f(paste0("H_1:", d(x$lower), "<D<", d(x$upper), ",\\qquad D\\approx", d(x$difference))))
  } else {
    boundary <- if (x$objective == "noninferiority") -x$margin else x$margin
    g <- x$direction_sign * x$difference
    gap <- g - boundary
    hypotheses <- c(f("g=tD"), f(if (x$objective == "noninferiority") "H_0:g\\le -M,\\qquad H_1:g>-M" else "H_0:g\\le M,\\qquad H_1:g>M"),
      f(if (x$objective == "noninferiority") "h=g+M" else "h=g-M"))
    hypothesis_text <- paste0("The direction factor $t$ is +1 when higher outcomes are better and -1 when lower outcomes are better. Here $M$ is the ",
      if (x$objective == "noninferiority") "maximum acceptable loss of benefit" else "required superiority margin (0 means any improvement)",
      "; $h$ is the expected distance from the null boundary.")
    rows <- c(rows, legend_row("t", "Direction factor for benefit in group 2", d(x$direction_sign)),
      legend_row("M", if (x$objective == "noninferiority") "Non-inferiority margin" else "Superiority margin", d(x$margin)),
      legend_row("g", "Expected benefit-oriented difference", d(g)), legend_row("h", "Distance from the null boundary", d(gap)))
    hypothesis_numeric <- c(f(paste0("g=", d(x$direction_sign), "\\times(", d(x$difference), ")\\approx", d(g))),
      f(paste0("H_0:g\\le", d(boundary), ",\\qquad H_1:g>", d(boundary))),
      f(paste0("h=(", d(g), ")-(", d(boundary), ")\\approx", d(gap))))
  }
  effect_generic <- effect_numeric <- character()
  if (proportions && x$effect_type != "proportions") {
    if (x$effect_type == "or") {
      effect_generic <- f("p_2=\\frac{OR\\,p_1}{1-p_1+OR\\,p_1}")
      effect_numeric <- f(paste0("p_2=\\frac{", d(x$effect), "\\times", d(x$p1), "}{1-", d(x$p1), "+", d(x$effect), "\\times", d(x$p1), "}\\approx", d(x$p2)))
      rows <- c(rows, legend_row("OR", "Odds ratio: group 2 / group 1", d(x$effect)))
    } else {
      effect_generic <- f("p_2=RR\\,p_1")
      effect_numeric <- f(paste0("p_2=", d(x$effect), "\\times", d(x$p1), "\\approx", d(x$p2)))
      rows <- c(rows, legend_row("RR", "Risk ratio: group 2 / group 1", d(x$effect)))
    }
  }
  se_generic <- f(if (proportions) "s=\\sqrt{p_1(1-p_1)/n_1+p_2(1-p_2)/n_2}" else "s=\\sqrt{\\sigma_1^2/n_1+\\sigma_2^2/n_2}")
  se_numeric <- f(if (proportions) paste0("s=\\sqrt{", d(x$p1), "(1-", d(x$p1), ")/", d(x$n1), "+", d(x$p2), "(1-", d(x$p2), ")/", d(x$n2), "}\\approx", d(x$se_alt)) else
    paste0("s=\\sqrt{(", d(x$sd1), ")^2/", d(x$n1), "+(", d(x$sd2), ")^2/", d(x$n2), "}\\approx", d(x$se_alt)))
  power_generic <- if (equivalence) c(f("a=(L-D)/s+Z_{1-\\alpha},\\qquad b=(U-D)/s-Z_{1-\\alpha}"),
    f("P_{\\mathrm{approx}}=\\max(0,\\Phi(b)-\\Phi(a))"),
    "Both tests must reject: power is the joint probability that the estimated difference lies between the two rejection boundaries, not the sum or product of separate powers.") else
    c(f("u=h/s-Z_{1-\\alpha},\\qquad P_{\\mathrm{approx}}=\\Phi(u)"), "This power calculation includes the single rejection tail specified by the direction of benefit.")
  generic_size <- if (!sizing) character() else if (equivalence) c(f("s(n)=\\sqrt{V/n}"),
    f("a(n)=(L-D)/s(n)+Z_{1-\\alpha}"), f("b(n)=(U-D)/s(n)-Z_{1-\\alpha}"),
    f("P(n)=\\max(0,\\Phi(b(n))-\\Phi(a(n)))"),
    f("n_{1,0}=\\inf\\{n>0:P(n)\\ge P_\\star\\}"),
    "The unrounded group 1 size is solved numerically for the requested joint power. This accommodates unequal allocation and asymmetric margins.") else
    c(f("n_{1,0}=\\frac{(Z_{1-\\alpha}+Z_{1-\\beta})^2V}{h^2}"))
  count_generic <- if (sizing) c(f("n_{2,0}=k n_{1,0},\\qquad n_{i,\\mathrm{adj}}=\\frac{n_{i,0}}{1-r}"),
    f("n_{\\mathrm{final}}=n_{\\mathrm{final},1}+n_{\\mathrm{final},2}"),
    "For $i=1,2$, $n_{i,0}$ is the unrounded complete size. The complete target $n_i$ and recruitment target $n_{\\mathrm{final},i}$ are rounded upward from the respective unrounded values. Recruitment inflation is applied before rounding.") else
    "The supplied $n_1,n_2$ are complete, analysable observations. No recruitment inflation is applied in power mode."
  critical <- c("### Significance and hypothesis", "", f("Z_{1-\\alpha}=\\Phi^{-1}(1-\\alpha)"),
    f(paste0("Z_{1-\\alpha}=\\Phi^{-1}(1-", d(x$alpha), ")\\approx", d(x$z_alpha))),
    effect_numeric, f(difference_numeric), hypothesis_numeric)
  if (sizing && !equivalence) critical <- c(critical, f("Z_{1-\\beta}=\\Phi^{-1}(P_\\star)"),
    f(paste0("Z_{1-\\beta}=\\Phi^{-1}(", d(x$power), ")\\approx", d(x$z_beta))))
  size_steps <- character()
  if (sizing) {
    size_steps <- c("### Complete sample sizes", "", f(variance_numeric))
    if (equivalence) {
      root_se <- sqrt(variance / x$n10)
      root_a <- (x$lower - x$difference) / root_se + x$z_alpha
      root_b <- (x$upper - x$difference) / root_se - x$z_alpha
      size_steps <- c(size_steps, f(paste0("P_\\star=", display_decimal(100 * x$power), "\\%")),
        f(paste0("n_{1,0}\\approx", d(x$n10))),
        f(paste0("s(n_{1,0})=\\sqrt{", d(variance), "/", d(x$n10), "}\\approx", d(root_se))),
        f(paste0("a(n_{1,0})\\approx\\frac{(", d(x$lower), ")-(", d(x$difference), ")}{", d(root_se), "}+", d(x$z_alpha), "\\approx", d(root_a))),
        f(paste0("b(n_{1,0})\\approx\\frac{(", d(x$upper), ")-(", d(x$difference), ")}{", d(root_se), "}-", d(x$z_alpha), "\\approx", d(root_b))),
        f(paste0("100P(n_{1,0})\\approx100[\\Phi(", d(root_b), ")-\\Phi(", d(root_a), ")]\\approx", display_decimal(100 * x$power), "\\%")))
    } else size_steps <- c(size_steps, f(paste0("n_{1,0}\\approx\\frac{(", d(x$z_alpha), "+", d(x$z_beta), ")^2\\times", d(variance), "}{(", d(gap), ")^2}\\approx", d(x$n10))))
    size_steps <- c(size_steps, f(paste0("n_{2,0}\\approx", d(x$ratio), "\\times", d(x$n10), "\\approx", d(x$n20))),
      f(paste0("n_1=", d(x$n1), ",\\qquad n_2=", d(x$n2))), "### Recruitment adjustment", "",
      f(paste0("n_{1,\\mathrm{adj}}\\approx\\frac{", d(x$n10), "}{1-", d(x$nonresponse), "}\\approx", d(x$adjusted1))),
      f(paste0("n_{2,\\mathrm{adj}}\\approx\\frac{", d(x$n20), "}{1-", d(x$nonresponse), "}\\approx", d(x$adjusted2))),
      f(paste0("n_{\\mathrm{final},1}=", d(x$final1), ",\\qquad n_{\\mathrm{final},2}=", d(x$final2))),
      f(paste0("n_{\\mathrm{final}}=", d(x$final1), "+", d(x$final2), "=", d(x$n_final))))
  } else size_steps <- c("### Supplied complete sizes", "", f(paste0("n_1=", d(x$n1), ",\\quad n_2=", d(x$n2), ",\\quad k=", d(x$n2), "/", d(x$n1), "\\approx", d(x$ratio))))
  power_steps <- c("### Approximate power at complete sizes", "", se_numeric)
  if (equivalence) {
    a <- (x$lower - x$difference) / x$se_alt + x$z_alpha
    b <- (x$upper - x$difference) / x$se_alt - x$z_alpha
    power_steps <- c(power_steps,
      f(paste0("a\\approx\\frac{(", d(x$lower), ")-(", d(x$difference), ")}{", d(x$se_alt), "}+", d(x$z_alpha), "\\approx", d(a))),
      f(paste0("b\\approx\\frac{(", d(x$upper), ")-(", d(x$difference), ")}{", d(x$se_alt), "}-", d(x$z_alpha), "\\approx", d(b))),
      f(paste0("100P_{\\mathrm{approx}}\\approx100\\max(0,\\Phi(", d(b), ")-\\Phi(", d(a), "))\\approx", display_decimal(100 * x$achieved_power), "\\%")))
  } else {
    u <- gap / x$se_alt - x$z_alpha
    power_steps <- c(power_steps, f(paste0("u\\approx", d(gap), "/", d(x$se_alt), "-", d(x$z_alpha), "\\approx", d(u))),
      f(paste0("100P_{\\mathrm{approx}}\\approx100\\Phi(", d(u), ")\\approx", display_decimal(100 * x$achieved_power), "\\%")))
  }
  power_steps <- c(power_steps, "Power results are displayed to at most two decimal places. All calculations retain full precision.")
  headline <- if (sizing) paste0("**Final recruitment target: ", display_count(x$n_final), " participants (group 1: ", display_count(x$final1), "; group 2: ", display_count(x$final2), ").**") else
    paste0("**Approximate power: ", display_power(x$achieved_power), " with group sizes ", display_count(x$n1), " and ", display_count(x$n2), ".**")
  interpretation <- if (sizing) paste0("For the specified ", tolower(objective_name(x$objective)), " hypothesis and target power ", display_power(x$power),
    ", complete targets are group 1: ", display_count(x$n1), " and group 2: ", display_count(x$n2),
    ". With ", display_percent(x$nonresponse), " non-response, recruitment targets are group 1: ", display_count(x$final1), " and group 2: ", display_count(x$final2),
    ", for a total of ", display_count(x$n_final), ". Approximate power at the rounded complete sizes is ", display_power(x$achieved_power), ".") else
    paste0("The specified ", tolower(objective_name(x$objective)), " hypothesis has approximate power ", display_power(x$achieved_power),
      " with ", display_count(x$n1), " complete observations in group 1 and ", display_count(x$n2), " in group 2.")
  assumptions <- c("- Groups and observations are independent. Paired data, clusters, and finite population corrections require other methods.",
    if (proportions) "- The directional and equivalence calculations use an unpooled Wald Z approximation at the expected probabilities. No continuity correction, constrained score test, or exact binomial calculation is applied." else
      "- Anticipated SDs are treated as known for normal-approximation planning. This is not an exact t-test or Welch calculation.",
    "- Clinical margins and expected differences must be specified before examining study results. Non-significance in a difference test does not establish non-inferiority or equivalence.",
    "- The displayed alpha is one-sided; for equivalence it applies to each of the two tests. The joint equivalence procedure controls alpha without dividing it between the two tests.",
    if (sizing) "- Complete and recruitment targets are independently rounded upward in each group. Rounding can change the exact requested allocation ratio." else "- Fixed-size power uses complete analysable counts; it does not account for losses or provide post hoc evidence about an observed effect.")
  if (length(x$warnings)) assumptions <- c(assumptions, paste0("- ", x$warnings))
  references <- c("1. ICH. *E9 Statistical principles for clinical trials*, sections 3.3 and 3.5. [Comparison objectives and margins](https://www.ema.europa.eu/en/documents/scientific-guideline/ich-e-9-statistical-principles-clinical-trials-step-5_en.pdf).",
    if (proportions) c("2. NCSS. *Non-inferiority tests for the difference between two proportions*. [Unpooled Z methods](https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Non-Inferiority_Tests_for_the_Difference_Between_Two_Proportions.pdf).",
      "3. NCSS. *Equivalence tests for the difference between two proportions*. [Two one-sided tests](https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Equivalence_Tests_for_the_Difference_Between_Two_Proportions.pdf).") else
      "2. statsmodels. *Normal power with specified variances*. [Normal approximation](https://www.statsmodels.org/stable/generated/statsmodels.stats.power.normal_power_het.html).")
  if (!proportions && x$sd_method != "separate") references <- c(references,
    "3. Penn State Department of Statistics. *STAT 500 Comparing two population parameters*. [Within-group pooled variance](https://online.stat.psu.edu/stat500/Lesson07).")
  paste(c(paste0("# ", if (sizing) "Sample size calculation for " else "Power calculation for ", subject), "", headline, "",
    paste0("Comparison objective: **", objective_name(x$objective), "**. This report states the planned hypothesis and documents its normal-approximation calculation."), "",
    "## Generic formulas", "", hypotheses, hypothesis_text, sd$generic, effect_generic, f(difference_formula), f(variance_formula),
    generic_size, count_generic, se_generic, power_generic, "The function $\\Phi$ is the standard-normal cumulative distribution function; $\\Phi^{-1}$ is its quantile function. The planning difference $D$ is signed, and $V$ is the variance factor.", "",
    "## Legends and input values", "", rows, "", "## Numerical substitution", "", sd$numeric, critical, size_steps, power_steps, "",
    "## Interpretation", "", interpretation, "", "## Assumptions and limitations", "", assumptions, "", "## References", "", references, ""), collapse = "\n")
}

yamane_markdown <- function(x) {
  f <- formula_lines
  d <- display_number
  lines <- c("# Sample size calculation using the Taro Yamane formula", "",
    paste0("**Final recruitment target: ", display_count(x$n_final), " participants.**"), "",
    "This report applies the simplified finite-population formula for planning a survey proportion. It assumes simple random sampling, approximately 95% confidence, and a proportion of 0.5.", "",
    "## Generic formulas", "", f("n_0=\\frac{N}{1+Ne^2},\\qquad n_{\\mathrm{adj}}=\\frac{n_0}{1-r}"),
    "The complete target $n_{\\mathrm{complete}}$ and recruitment target $n_{\\mathrm{final}}$ are rounded upward from their respective unrounded values.", "",
    "## Legends and input values", "", legend_header(), legend_row("N", "Finite population size", d(x$population)),
    legend_row("e", "Absolute precision fraction", paste0(d(x$precision), " (", display_number(100 * x$precision), " percentage points)")),
    legend_row("r", "Expected non-response fraction", d(x$nonresponse)), legend_row("n_0", "Unrounded complete sample size", d(x$n0)),
    legend_row("n_{\\mathrm{adj}}", "Unrounded recruitment size", d(x$n_adjusted)),
    legend_row("n_{\\mathrm{complete}}", "Rounded complete sample target", d(x$n_complete)),
    legend_row("n_{\\mathrm{final}}", "Rounded recruitment target", d(x$n_final)), "",
    "## Numerical substitution", "", f(paste0("e=", d(100 * x$precision), "/100=", d(x$precision))),
    f(paste0("n_0=\\frac{", d(x$population), "}{1+", d(x$population), "\\times(", d(x$precision), ")^2}\\approx", d(x$n0))),
    f(paste0("n_{\\mathrm{complete}}=", d(x$n_complete))),
    f(paste0("n_{\\mathrm{adj}}\\approx\\frac{", d(x$n0), "}{1-", d(x$nonresponse), "}\\approx", d(x$n_adjusted))),
    f(paste0("n_{\\mathrm{final}}=", d(x$n_final))),
    "Calculations retain full precision; non-response inflation is applied before final rounding.", "",
    "## Interpretation", "", paste0("For a population of ", display_count(x$population), " and absolute precision of ", display_number(100 * x$precision),
      " percentage points, the simplified formula gives ", display_count(x$n_complete), " complete observations. Allowing for ", display_percent(x$nonresponse),
      " non-response gives a recruitment target of ", display_count(x$n_final), "."), "",
    "## Assumptions and limitations", "", "- This is a simplified survey-proportion calculation; it is not a calculation for means, group comparisons, or diagnostic accuracy.",
    "- Simple random sampling, approximately 95% confidence, and an assumed proportion of 0.5 underlie this formula. Confidence and the expected proportion are not adjustable inputs.",
    "- The finite population size must be known. No cluster design effect is included.",
    "- The simplified formula approximates precision and should not replace a design-specific calculation.",
    "- Non-response inflation allows for expected loss of observations; it does not remove selection bias.")
  if (length(x$warnings)) lines <- c(lines, "", "### Checks for these inputs", "", paste0("- ", x$warnings))
  paste(c(lines, "", "## References", "",
    "1. Yamane T. *Statistics An Introductory Analysis*. 2nd ed. New York: Harper and Row; 1967. p. 886.",
    "2. Israel GD. *Determining sample size*. University of Florida IFAS Extension, PEOD6. [Simplified formula for proportions and its assumptions](https://ask.ifas.ufl.edu/publication/PD006).", ""), collapse = "\n")
}
