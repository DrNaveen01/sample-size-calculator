#!/usr/bin/env python3
"""Apply targeted changes to the existing sample-size-calculator checkout.

Requires the repository sources already to be present; does not clone, install
packages, change Git settings, commit, or deploy. Fails on upstream drift.
"""
from pathlib import Path
import argparse

ROOT = Path.cwd()
MODIFIED = {}

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--check', action='store_true',
                    help='Verify transformations against a repository checkout without writing')
args = parser.parse_args()

def edit(path):
    p = ROOT / path
    if not p.is_file():
        raise RuntimeError(f"Missing original repository file: {path}")
    return p.read_text(encoding="utf-8")

def change(path, original, before, after):
    if original.count(before) != 1:
        raise RuntimeError(f"Expected exactly one occurrence in {path}, got {original.count(before)}: {before[:110]!r}")
    return original.replace(before, after, 1)

def save(path, contents):
    MODIFIED[path] = contents

p="R/single_proportion.R"; s=edit(p)
s=change(p,s, "#' Estimate sample size for one population proportion", '''# Resolve an optional direct variance inflation factor or equal-cluster-size ICC model.
# An ICC must be paired with mean within-cluster size M (which may be fractional).
resolve_design_effect <- function(design_effect = NULL, icc = NULL, cluster_size = NULL) {
  if (!is.null(design_effect) && (!is.null(icc) || !is.null(cluster_size)))
    stop("Choose either direct design effect or ICC and mean cluster size, not both.", call. = FALSE)
  if (xor(is.null(icc), is.null(cluster_size)))
    stop("Provide ICC and mean cluster size together.", call. = FALSE)
  if (!is.null(icc)) {
    check_number(icc, "Intracluster correlation (ICC)", 0, 1)
    check_number(cluster_size, "Mean cluster size M", 1, Inf)
    value <- 1 + (cluster_size - 1) * icc
    source <- "icc"
  } else if (!is.null(design_effect)) {
    check_number(design_effect, "Design effect", 1, Inf)
    value <- design_effect
    source <- "direct"
  } else {
    value <- 1
    source <- "none"
  }
  if (!is.finite(value)) stop("Computed design effect is not finite.", call. = FALSE)
  list(value = value, source = source, icc = icc, cluster_size = cluster_size)
}

finite_population_size <- function(population_size) {
  if (is.null(population_size)) return(NULL)
  check_number(population_size, "Finite population size", 1, 2^53 - 1)
  if (population_size != floor(population_size))
    stop("Finite population size must be an integer.", call. = FALSE)
  population_size
}

#' Estimate sample size for one population proportion''')
s=change(p,s, '                             z = NULL) {', '''                             z = NULL, design_effect = NULL, icc = NULL,
                             cluster_size = NULL, population_size = NULL) {''')
s=change(p,s, '  n0 <- (z_value * sqrt(p * (1 - p)) / d)^2\n  adjusted <- n0 / (1 - nonresponse)', '''  design <- resolve_design_effect(design_effect, icc, cluster_size)
  population_size <- finite_population_size(population_size)
  n_srs <- (z_value * sqrt(p * (1 - p)) / d)^2
  n_design <- n_srs * design$value
  # Without-replacement FPC, using the same variance inflation in the numerator.
  n0 <- if (is.null(population_size)) n_design else
    n_design * population_size / (population_size - 1 + n_design)
  adjusted <- n0 / (1 - nonresponse)''')
s=change(p,s, '  if (base * p < 10 || base * (1 - p) < 10) {', '''  if (!is.null(population_size) && final > population_size) {
    warnings <- c(warnings, "Recruitment exceeds the finite sampling frame; the chosen precision and expected loss may be infeasible.")
  }
  if (design$value > 1) warnings <- c(warnings,
    "The equal-cluster-size design effect is a first-order approximation; the number of clusters, variable cluster sizes and analysis method require separate planning.")
  if (base * p < 10 || base * (1 - p) < 10) {''')
s=change(p,s, '    z_source = z_source, nonresponse = nonresponse,\n    n0 = n0,', '''    z_source = z_source, nonresponse = nonresponse,
    design_effect = design$value, design_source = design$source,
    icc = design$icc, cluster_size = design$cluster_size,
    population_size = population_size, n_srs = n_srs, n_design = n_design,
    n0 = n0,''')
save(p,s)

p="R/planning.R"; s=edit(p)
s=change(p,s, 'single_mean <- function(sd, precision, confidence = 0.95, nonresponse = 0, z = NULL) {', '''single_mean <- function(sd, precision, confidence = 0.95, nonresponse = 0, z = NULL,
                        design_effect = NULL, icc = NULL, cluster_size = NULL,
                        population_size = NULL) {''')
s=change(p,s, '  n0 <- (critical$z * sd / precision)^2\n  adjusted <- n0 / (1 - nonresponse)', '''  design <- resolve_design_effect(design_effect, icc, cluster_size)
  population_size <- finite_population_size(population_size)
  n_srs <- (critical$z * sd / precision)^2
  n_design <- n_srs * design$value
  n0 <- if (is.null(population_size)) n_design else
    n_design * population_size / (population_size - 1 + n_design)
  adjusted <- n0 / (1 - nonresponse)''')
s=change(p,s, '  structure(c(list(method = "Normal approximation for one population mean", sd = sd,', '''  if (!is.null(population_size) && ceiling(adjusted) > population_size)
    warnings <- c(warnings, "Recruitment exceeds the finite sampling frame; the chosen precision and expected loss may be infeasible.")
  if (design$value > 1) warnings <- c(warnings,
    "Equal-cluster-size design effect is an approximation; separately check the number of clusters and analysis design.")
  structure(c(list(method = "Normal approximation for one population mean", sd = sd,''')
s=change(p,s, '    d = precision, precision = precision, nonresponse = nonresponse, n0 = n0,', '''    d = precision, precision = precision, nonresponse = nonresponse,
    design_effect = design$value, design_source = design$source,
    icc = design$icc, cluster_size = design$cluster_size,
    population_size = population_size, n_srs = n_srs, n_design = n_design, n0 = n0,''')
s=change(p,s, '                            lower = NULL, upper = NULL, group1 = "Group 1", group2 = "Group 2") {', '''                            lower = NULL, upper = NULL, group1 = "Group 1", group2 = "Group 2",
                            design_effect = NULL, icc = NULL, cluster_size = NULL) {''')
s=change(p,s, '  groups <- comparison_group_names(group1, group2)\n  effect_type <- match.arg(effect_type)', '''  groups <- comparison_group_names(group1, group2)
  design <- resolve_design_effect(design_effect, icc, cluster_size)
  effect_type <- match.arg(effect_type)''')
s=change(p,s, '  counts <- comparison_counts(n10, settings, n1, n2)\n  # Recompute the pooled proportion', '''  n10_srs <- n10
  if (settings$mode == "sample_size") n10 <- n10 * design$value
  counts <- comparison_counts(n10, settings, n1, n2)
  # Recompute the pooled proportion''')
s=change(p,s, '  if (settings$objective != "equality") se_null <- se_alt\n  achieved <-', '''  se_null <- se_null * sqrt(design$value)
  se_alt <- se_alt * sqrt(design$value)
  if (settings$objective != "equality") se_null <- se_alt
  achieved <-''')
s=change(p,s, '    achieved_power = achieved, warnings = warnings), groups, settings, counts),', '''    achieved_power = achieved, warnings = warnings,
    design_effect = design$value, design_source = design$source,
    icc = design$icc, cluster_size = design$cluster_size, n10_srs = n10_srs),
    groups, settings, counts),''')
s=change(p,s, 'two_means <- function(mean1, mean2, sd1 = NULL, sd2 = sd1, power = 0.80,', '''two_means <- function(mean1 = NULL, mean2 = NULL, sd1 = NULL, sd2 = sd1, power = 0.80,''')
s=change(p,s, '                      ref_n1 = NULL, ref_n2 = NULL, group1 = "Group 1", group2 = "Group 2") {\n  groups <- comparison_group_names(group1, group2)\n  check_number(mean1, "Group 1 mean", -Inf, Inf)\n  check_number(mean2, "Group 2 mean", -Inf, Inf)', '''                      ref_n1 = NULL, ref_n2 = NULL, group1 = "Group 1", group2 = "Group 2",
                      mean_difference = NULL, design_effect = NULL, icc = NULL,
                      cluster_size = NULL) {
  groups <- comparison_group_names(group1, group2)
  design <- resolve_design_effect(design_effect, icc, cluster_size)
  mean_input <- if (is.null(mean_difference)) "means" else "difference"
  if (mean_input == "difference") {
    check_number(mean_difference, "Signed mean difference (group 2 minus group 1)", -Inf, Inf)
    if (!is.null(mean1) || !is.null(mean2))
      stop("Enter either group means or their signed difference, not both.", call. = FALSE)
    # Centering at zero is internal to the planning computation, not an imputation
    # of either group's anticipated mean. Reports explicitly show the difference.
    mean1 <- 0; mean2 <- mean_difference
  } else {
    check_number(mean1, "Group 1 mean", -Inf, Inf)
    check_number(mean2, "Group 2 mean", -Inf, Inf)
  }''')
s=change(p,s, '  counts <- comparison_counts(n10, settings, n1, n2)\n  se <- sqrt(sd1^2 / counts$n1 + sd2^2 / counts$n2)', '''  n10_srs <- n10
  if (settings$mode == "sample_size") n10 <- n10 * design$value
  counts <- comparison_counts(n10, settings, n1, n2)
  se <- sqrt(design$value * (sd1^2 / counts$n1 + sd2^2 / counts$n2))''')
s=change(p,s, '    pooled_sd = pooled_sd, ref_n1 = ref_n1, ref_n2 = ref_n2,\n    se_null = se', '''    pooled_sd = pooled_sd, ref_n1 = ref_n1, ref_n2 = ref_n2,
    mean_input = mean_input,
    design_effect = design$value, design_source = design$source,
    icc = design$icc, cluster_size = design$cluster_size, n10_srs = n10_srs,
    se_null = se''')
save(p,s)

p="R/plots.R"; s=edit(p)
s=change(p,s, '    power=if(is.null(x$power)) .8 else x$power,ratio=x$ratio,nonresponse=0)',
  '''    power=if(is.null(x$power)) .8 else x$power,ratio=x$ratio,nonresponse=0)
  if (identical(x$design_source, "direct")) shared$design_effect <- x$design_effect
  if (identical(x$design_source, "icc")) {
    shared$icc <- x$icc
    shared$cluster_size <- x$cluster_size
  }''')
save(p,s)

p="R/app_ui.R"; s=edit(p)
s=change(p,s, '            shiny::numericInput("mean1", "Group 1 expected mean", 100, step = 1),\n            shiny::numericInput("mean2", "Group 2 expected mean", 105, step = 1),', '''            shiny::radioButtons("mean_input", "Specify expected effect using",
              c("Both group means" = "means", "Signed difference (group 2 minus group 1)" = "difference"),
              selected = "means", inline = TRUE),
            shiny::conditionalPanel("input.mean_input === 'means'",
              shiny::numericInput("mean1", "Group 1 expected mean", 100, step = 1),
              shiny::numericInput("mean2", "Group 2 expected mean", 105, step = 1)),
            shiny::conditionalPanel("input.mean_input === 'difference'",
              shiny::numericInput("mean_difference", "Expected signed difference (group 2 minus group 1)", 5, step = .5)),''')
s=change(p,s, '          shiny::conditionalPanel("input.calculator === \'single_proportion\' || input.calculator === \'single_mean\'",\n            shiny::tags$div(class = "form-divider"),', '''          shiny::conditionalPanel("['single_proportion','single_mean','two_proportions','two_means'].includes(input.calculator)",
            shiny::tags$div(class = "form-divider"),
            shiny::radioButtons("apply_design_effect", "Apply design effect?",
              choices = c("No" = "no", "Yes" = "yes"), selected = "no", inline = TRUE),
            shiny::conditionalPanel("input.apply_design_effect === 'yes'",
              shiny::selectInput("design_mode", "Specify design effect using",
                c("Enter DEFF directly" = "direct", "Estimate from cluster size and ICC" = "icc"),
                selected = "direct", selectize = FALSE),
              shiny::conditionalPanel("input.design_mode === 'direct'",
                shiny::numericInput("design_effect", "Design effect (DEFF)", 1.5, min = 1, step = .1)),
              shiny::conditionalPanel("input.design_mode === 'icc'",
                shiny::numericInput("cluster_icc", "Intracluster correlation (ICC)", .02, min = 0, max = 1, step = .01),
                shiny::numericInput("cluster_m", "Mean participants per cluster (M)", 20, min = 1, step = 1),
                shiny::tags$p(class = "field-help", "DEFF = 1 + (M - 1) × ICC; assumes approximately equal cluster sizes.")),
              shiny::tags$p(class = "field-help", "Design effect inflates planning variances. For cluster trials, separately verify the number of clusters and the analysis strategy."))),
          shiny::conditionalPanel("input.calculator === 'single_proportion' || input.calculator === 'single_mean'",
            shiny::radioButtons("apply_fpc", "Apply finite population correction?",
              choices = c("No" = "no", "Yes" = "yes"), selected = "no", inline = TRUE),
            shiny::conditionalPanel("input.apply_fpc === 'yes'",
              shiny::numericInput("fpc_population", "Eligible finite population size (N)", 1000, min = 1, step = 1),
              shiny::tags$p(class = "field-help", "Use only for sampling without replacement from a known finite eligible population; not routine randomized trials."))),
          shiny::conditionalPanel("input.calculator === 'single_proportion' || input.calculator === 'single_mean'",
            shiny::tags$div(class = "form-divider"),''')
save(p,s)

p="R/app_server.R"; s=edit(p)
s=change(p,s, '      x <- switch(type,', '''      # Disabled adjustments are not passed to the calculation engines, even if
      # the hidden inputs retain their previous values after switching to No.
      design_args <- if (type %in% c("single_proportion","single_mean","two_proportions","two_means") &&
                         identical(input$apply_design_effect, "yes"))
        switch(input$design_mode,
          direct = list(design_effect = input$design_effect),
          icc = list(icc = input$cluster_icc, cluster_size = input$cluster_m),
          stop("Choose direct DEFF or cluster size and ICC.", call. = FALSE)) else list()
      finite_args <- if (type %in% c("single_proportion", "single_mean") &&
                         identical(input$apply_fpc, "yes"))
        list(population_size = input$fpc_population) else list()
      x <- switch(type,''')
s=change(p,s, '''          single_proportion(input$p_pct / 100,
            if (relative) input$relative_pct / 100 else input$absolute_pct / 100,
            confidence, nonresponse, input$precision_type, z)''', '''          do.call(single_proportion, c(list(p = input$p_pct / 100,
            precision = if (relative) input$relative_pct / 100 else input$absolute_pct / 100,
            confidence = confidence, nonresponse = nonresponse,
            precision_type = input$precision_type, z = z), design_args, finite_args))''')
s=change(p,s, '        single_mean = single_mean(input$mean_sd, input$mean_precision, confidence, nonresponse, z),', '''        single_mean = do.call(single_mean, c(list(sd = input$mean_sd,
          precision = input$mean_precision, confidence = confidence,
          nonresponse = nonresponse, z = z), design_args, finite_args)),''')
s=change(p,s, '            effect = if (input$effect_type == "or") input$odds_ratio else if (input$effect_type == "rr") input$risk_ratio else NULL), comparison_args))', '''            effect = if (input$effect_type == "or") input$odds_ratio else if (input$effect_type == "rr") input$risk_ratio else NULL), comparison_args, design_args))''')
s=change(p,s, '''        two_means = do.call(two_means, c(list(mean1 = input$mean1, mean2 = input$mean2,
          sd1 = input$sd1, sd2 = input$sd2,''', '''        two_means = do.call(two_means, c(list(
          mean1 = if (identical(input$mean_input, "difference")) NULL else input$mean1,
          mean2 = if (identical(input$mean_input, "difference")) NULL else input$mean2,
          mean_difference = if (identical(input$mean_input, "difference")) input$mean_difference else NULL,
          sd1 = input$sd1, sd2 = input$sd2,''')
s=change(p,s, '          pooled_sd = input$pooled_sd, ref_n1 = input$ref_n1, ref_n2 = input$ref_n2), comparison_args)),', '''          pooled_sd = input$pooled_sd, ref_n1 = input$ref_n1, ref_n2 = input$ref_n2), comparison_args, design_args)),''')
s=change(p,s, '      if (inherits(x, "two_means_result") && x$sd_method != "separate")', '''      if (!is.null(x$design_effect) && !identical(x$design_source, "none"))
        shiny::tags$p(class = "power-check", paste("Applied design effect:", display_number(x$design_effect))),
      if (!is.null(x$population_size))
        shiny::tags$p(class = "power-check", paste("Finite sampling frame:", display_count(x$population_size))),
      if (inherits(x, "two_means_result") && x$sd_method != "separate")''')
s=change(p,s, '      ref_n1 = 50, ref_n2 = 100, population_size = 1000, yamane_precision_pct = 5)', '''      ref_n1 = 50, ref_n2 = 100, population_size = 1000, yamane_precision_pct = 5,
      mean_difference = 5, design_effect = 1.5, cluster_icc = .02, cluster_m = 20,
      fpc_population = 1000)''')
s=change(p,s, '    shiny::updateSelectInput(session, "sd_method", selected = "separate")', '''    shiny::updateSelectInput(session, "sd_method", selected = "separate")
    shiny::updateRadioButtons(session, "apply_design_effect", selected = "no")
    shiny::updateSelectInput(session, "design_mode", selected = "direct")
    shiny::updateRadioButtons(session, "apply_fpc", selected = "no")
    shiny::updateRadioButtons(session, "mean_input", selected = "means")''')
save(p,s)

p="R/advanced_ui.R"; s=edit(p)
s=change(p,s, "panel(\"input.auc_objective === 'test'\",num(\"auc_null\",\"Null AUC\",.5,.001,.999)),",
  "panel(\"input.auc_objective === 'test'\",num(\"auc_null\",\"Null AUC\",.5,.001,.999),\n        sel(\"auc_method\",\"Single-AUC calculation method\",c(\"Cleveland Clinic RiskCalc / Lu (2021)\"=\"riskcalc\",\"Hanley-McNeil variance (existing)\"=\"hanley\"))),")
s=change(p,s, 'sel("auc_objective","AUC objective",c("Estimate one AUC"="estimate","Test one AUC"="test","Compare independent AUCs"="independent","Compare paired AUCs"="paired")),', '''sel("auc_objective","AUC objective",c("Test one AUC against a null value"="test","Estimate one AUC"="estimate","Compare independent AUCs"="independent","Compare paired AUCs"="paired")),''')
s=change(p,s, 'sel("auc_recruitment","Recruitment design",c("Separate disease strata"="separate","One population using prevalence"="population")),', '''sel("auc_recruitment","Recruitment design",c("One population using expected prevalence"="population","Separate disease strata"="separate")),''')
s=change(p,s, 'shiny::tags$p(class="method-note","Full-AUC Hanley–McNeil variance approximation. Paired tests share participants; independent tests use separate cohorts.")', '''shiny::tags$p(class="method-note","For one-AUC testing, select RiskCalc (Lu logit-AUC method) or the original Hanley–McNeil approximation. Enter prevalence, null AUC 0.5, alpha and power. Reports give diseased, non-diseased and total targets.")''')
s=change(p,s, "panel(\"input.auc_recruitment === 'population'\",num(\"auc_prevalence\",\"Disease prevalence (%)\",20,.001,99.999,1)),",
  "panel(\"input.auc_recruitment === 'population' || (input.auc_objective === 'test' && input.auc_method === 'riskcalc')\",num(\"auc_prevalence\",\"Disease prevalence (%)\",20,.001,99.999,1)),")
s=change(p,s, "panel(\"input.auc_recruitment === 'separate'\",num(\"auc_ratio\",\"Non-diseased / diseased size ratio\",1,.001,NULL,.1))),",
  "panel(\"input.auc_recruitment === 'separate' && !(input.auc_objective === 'test' && input.auc_method === 'riskcalc')\",num(\"auc_ratio\",\"Non-diseased / diseased size ratio\",1,.001,NULL,.1))),")
s=change(p,s, "  if(type==\"auc\") return(do.call(auc_sample,c(list(auc=input$auc_value,objective=input$auc_objective,",
  "  if(type==\"auc\") return(do.call(auc_sample,c(list(auc=input$auc_value,objective=input$auc_objective,\n    auc_method=if(input$auc_objective==\"test\" && identical(input$auc_method,\"riskcalc\")) \"riskcalc\" else \"hanley\",")
s=change(p,s, '    cohort_ratio=input$auc_cohort_ratio,recruitment=if(fixed) "separate" else input$auc_recruitment,prevalence=input$auc_prevalence/100,',
  '    cohort_ratio=input$auc_cohort_ratio,recruitment=if(fixed) "separate" else if(input$auc_objective=="test" && identical(input$auc_method,"riskcalc")) "population" else input$auc_recruitment,prevalence=input$auc_prevalence/100,')
save(p,s)

p="R/app_server.R"; s=MODIFIED[p]
s=change(p,s, 'advanced_selections <- list(advanced_mode="sample_size",advanced_alternative="two.sided",auc_objective="estimate",auc_recruitment="separate",', '''advanced_selections <- list(advanced_mode="sample_size",advanced_alternative="two.sided",auc_objective="test",auc_recruitment="population",''')
s=change(p,s, '    for(name in names(advanced_selections)) shiny::updateSelectInput(session,name,selected=advanced_selections[[name]])',
  '    for(name in names(advanced_selections)) shiny::updateSelectInput(session,name,selected=advanced_selections[[name]])\n    shiny::updateSelectInput(session,"auc_method",selected="riskcalc")')
save(p,s)

p="R/advanced.R"; s=edit(p)
s=change(p,s, 'auc_sample <- function(auc = .8, objective = c("estimate","test","independent","paired"),',
  r'''# Reproduces the logit-AUC expression in the Cleveland Clinic RiskCalc R source.
# The RiskCalc implementation allocates ceiling(n * prevalence) diseased and
# ceiling(n * (1 - prevalence)) non-diseased observations separately.
auc_riskcalc_sample <- function(auc, null_auc, prevalence, alpha, power,
                                alternative, nonresponse, cases = NULL,
                                controls = NULL) {
  probability(auc, "Anticipated AUC")
  probability(null_auc, "Null AUC")
  probability(prevalence, "Disease prevalence")
  check_number(alpha, "Two-sided significance level", 0, .5, TRUE, TRUE)
  check_number(nonresponse, "Non-response fraction", 0, 1, FALSE, TRUE)
  if (alternative != "two.sided")
    stop("The RiskCalc AUC formula requires a two-sided alternative; use Hanley-McNeil for a directional test.", call. = FALSE)
  fixed <- !is.null(cases) || !is.null(controls)
  if (!fixed) check_number(power, "Target power", .5, 1, TRUE, TRUE)
  if (xor(is.null(cases), is.null(controls)))
    stop("For fixed power, provide both diseased and non-diseased counts.", call. = FALSE)
  if (fixed) {
    whole_count(cases, "Complete diseased count")
    whole_count(controls, "Complete non-diseased count")
    prevalence <- cases / (cases + controls)
    nonresponse <- 0
  }
  logit_gap <- qlogis(auc) - qlogis(null_auc)
  if (!fixed && abs(logit_gap) < .Machine$double.eps)
    stop("Anticipated and null AUC must differ for sample size planning.", call. = FALSE)
  # Here k denotes diseased:non-diseased odds, not the inverse allocation.
  k <- prevalence / (1 - prevalence)
  variance_factor <- function(theta, ratio) {
    (1 + ratio) * (theta / (ratio * (2 - theta)) +
      2 * theta^2 / (1 + theta) - theta^2 * (1 / ratio + 1)) /
      (theta^2 * (1 - theta)^2)
  }
  v <- variance_factor(auc, k)
  if (!is.finite(v) || v <= 0)
    stop("Unsupported AUC/prevalence combination for the RiskCalc variance approximation.", call. = FALSE)
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- if (fixed) NULL else stats::qnorm(power)
  n_unrounded <- if (fixed) NULL else (z_alpha + z_beta)^2 * v / logit_gap^2
  if (!fixed) {
    check_planned_count(n_unrounded)
    m <- ceiling(n_unrounded * prevalence)
    n <- ceiling(n_unrounded * (1 - prevalence))
  } else { m <- cases; n <- controls }
  total <- m + n
  check_planned_count(c(m, n, total, ceiling(total / (1 - nonresponse))))
  realised_ratio <- m / n
  v_real <- variance_factor(auc, realised_ratio)
  achieved <- stats::pnorm(abs(logit_gap) * sqrt(total / v_real) - z_alpha)
  recruitment_total <- ceiling(total / (1 - nonresponse))
  counts <- data.frame(label = "Population cohort", complete = total,
                       recruitment = recruitment_total)
  strata <- data.frame(label = c("Diseased", "Non-diseased"), complete = c(m, n))
  d <- display_number
  formulas <- c(
    "\\ell(A)=\\log\\left(\\frac{A}{1-A}\\right),\\quad k=\\frac{\\pi}{1-\\pi}",
    "V=\\frac{1+k}{A^2(1-A)^2}\\left[\\frac{A}{k(2-A)}+\\frac{2A^2}{1+A}-A^2(1+1/k)\\right]",
    "n_0=\\frac{(Z_{1-\\alpha/2}+Z_{1-\\beta})^2V}{[\\ell(A)-\\ell(A_0)]^2}",
    "n_D=\\lceil n_0\\pi\\rceil,\\quad n_{ND}=\\lceil n_0(1-\\pi)\\rceil",
    "N_{\\mathrm{complete}}=n_D+n_{ND},\\quad N_{\\mathrm{final}}=\\left\\lceil\\frac{N_{\\mathrm{complete}}}{1-r}\\right\\rceil")
  steps <- c(
    paste0("k=", d(prevalence), "/(1-", d(prevalence), ")\\approx", d(k)),
    paste0("\\ell(A)-\\ell(A_0)=\\logit(", d(auc), ")-\\logit(",
           d(null_auc), ")\\approx", d(logit_gap)),
    paste0("V=\\frac{1+", d(k), "}{(", d(auc), ")^2(1-", d(auc),
           ")^2}\\left[\\frac{", d(auc), "}{", d(k), "(2-", d(auc),
           ")}+\\frac{2(", d(auc), ")^2}{1+", d(auc),
           "}-(", d(auc), ")^2(1+1/", d(k), ")\\right]\\approx", d(v)))
  if (!fixed) steps <- c(steps,
    paste0("Z_{1-\\alpha/2}=", d(z_alpha), ",\\quad Z_{1-\\beta}=", d(z_beta)),
    paste0("n_0=\\frac{(", d(z_alpha), "+", d(z_beta), ")^2\\times",
           d(v), "}{(", d(logit_gap), ")^2}\\approx", d(n_unrounded)),
    paste0("n_D=\\lceil", d(n_unrounded), "\\times", d(prevalence),
           "\\rceil=", m, ",\\quad n_{ND}=\\lceil", d(n_unrounded),
           "\\times(1-", d(prevalence), ")\\rceil=", n))
  steps <- c(steps,
    paste0("N_{\\mathrm{complete}}=", m, "+", n, "=", total),
    paste0("N_{\\mathrm{final}}=\\left\\lceil", total, "/(1-",
           d(nonresponse), ")\\right\\rceil=", recruitment_total),
    paste0("P_{\\mathrm{approx}}=\\Phi\\left(\\frac{|", d(logit_gap),
           "|}{\\sqrt{", d(v_real), "/", total, "}}-", d(z_alpha),
           "\\right)\\approx", d(achieved)))
  inputs <- c(A = auc, A_0 = null_auc, pi = prevalence, alpha = alpha,
              if (!fixed) c(P_star = power), r = nonresponse)
  args <- list(auc = auc, objective = "test", null_auc = null_auc,
    alpha = alpha, power = if (fixed) .8 else power, alternative = alternative,
    prevalence = prevalence, recruitment = "population", ratio = n / m,
    cases = if (fixed) cases else NULL, controls = if (fixed) controls else NULL,
    nonresponse = nonresponse, auc_method = "riskcalc")
  x <- extended_result("auc", "ROC AUC hypothesis testing",
    "RiskCalc / Lu (2021) logit-AUC normal approximation", inputs, formulas,
    steps, c(A = "Anticipated AUC", A_0 = "Null hypothesis AUC",
      pi = "Expected disease prevalence", alpha = "Two-sided type I error",
      P_star = "Target power", r = "Non-response fraction"), counts,
    c("Uses the Cleveland Clinic RiskCalc logit-transformed AUC expression; a normal large-sample approximation, not a DeLong test.",
      "Two-sided alpha with the main rejection tail approximation. The distant tail is omitted in the closed-form sizing and power check.",
      "Separate upward rounding of disease-positive and disease-negative quotas reproduces RiskCalc's allocation convention.",
      "Prevalence governs expected recruitment composition, not guaranteed strata. Verify adequate diseased and non-diseased accrual.",
      "Independent reference-standard confirmed disease-status strata and an anticipated full ROC AUC are assumed."),
    c("Cleveland Clinic RiskCalc. Sample-size source: https://github.com/ClevelandClinicQHS/riskcalc-website/blob/main/samplesize/samplesize.R",
      "Lu G. Sample size formulas for estimating areas under the receiver operating characteristic curves with precision and assurance. Western University thesis. 2021. https://ir.lib.uwo.ca/etd/8045",
      "Wang X, Ji X. Chest. 2020;158:S12-S20. https://doi.org/10.1016/j.chest.2020.03.010"),
    args, list(objective = "test", mode = if (fixed) "power" else "sample_size",
      achieved_power = achieved, power = if (fixed) NULL else power,
      strata = strata, required_diseased = m, required_nondiseased = n,
      expected_diseased = if (fixed) NULL else total * prevalence,
      expected_nondiseased = if (fixed) NULL else total * (1 - prevalence),
      ratio = n / m, nonresponse = nonresponse, confidence = NULL,
      se_alt = sqrt(v_real / total), n_unrounded = n_unrounded))
  x
}''' + '\n\nauc_sample <- function(auc = .8, objective = c("estimate","test","independent","paired"),')
s=change(p,s, '                       group1 = "Test 1", group2 = "Test 2") {',
  '                       group1 = "Test 1", group2 = "Test 2", auc_method = c("hanley", "riskcalc")) {')
s=change(p,s, '  objective <- match.arg(objective); recruitment <- match.arg(recruitment)',
  '''  objective <- match.arg(objective); recruitment <- match.arg(recruitment)
  auc_method <- match.arg(auc_method)
  if (objective == "test" && auc_method == "riskcalc") {
    return(auc_riskcalc_sample(auc = auc, null_auc = null_auc,
      prevalence = prevalence, alpha = alpha, power = power,
      alternative = alternative, nonresponse = nonresponse,
      cases = cases, controls = controls))
  }
  if (objective != "test" && auc_method == "riskcalc")
    stop("RiskCalc AUC formula supports only testing one AUC against a null AUC.", call. = FALSE)''')
s=change(p,s, '  if(estimation && any(result$anticipated_interval < 0 | result$anticipated_interval > 1)) result$warnings <- "The anticipated normal interval extends beyond [0,1]; consider simulation or a transformed AUC interval."', '''  # Expose minimum complete diagnostic strata and population expectations separately.
  result$required_diseased <- cc[1]
  result$required_nondiseased <- cc[2]
  if (objective %in% c("test", "estimate") && recruitment == "population" && !fixed) {
    result$expected_diseased <- result$n_complete * prevalence
    result$expected_nondiseased <- result$n_complete * (1 - prevalence)
  }
  if(estimation && any(result$anticipated_interval < 0 | result$anticipated_interval > 1)) result$warnings <- "The anticipated normal interval extends beyond [0,1]; consider simulation or a transformed AUC interval."''')
save(p,s)

p="R/advanced_ui.R"; s=MODIFIED[p]
s=change(p,s, '    shiny::tags$div(class="result-details",detail("Complete total",display_count(x$n_complete)),', '''    if(x$key == "auc" && x$objective %in% c("test", "estimate"))
      shiny::tags$div(class="group-targets",
        shiny::tags$div(shiny::tags$span("Required disease-positive cases"),
          shiny::tags$strong(display_count(x$required_diseased))),
        shiny::tags$div(shiny::tags$span("Required disease-negative cases"),
          shiny::tags$strong(display_count(x$required_nondiseased))),
        shiny::tags$div(shiny::tags$span("Total complete participants"),
          shiny::tags$strong(display_count(x$n_complete)))),
    shiny::tags$div(class="result-details",detail("Complete total",display_count(x$n_complete)),''')
save(p,s)

p="R/reports_advanced.R"; s=edit(p)
s=change(p,s, '  if(!is.null(x$strata)) {', '''  if(x$key == "auc" && x$objective %in% c("test", "estimate")) {
    lines <- c(lines, "", "### Disease-status recruitment plan", "",
      paste0("- Minimum complete disease-positive cases: **", display_count(x$required_diseased), "**."),
      paste0("- Minimum complete disease-negative cases: **", display_count(x$required_nondiseased), "**."),
      paste0("- Total complete participant target: **", display_count(x$n_complete), "**."),
      paste0("- Final recruitment target allowing for non-response: **", display_count(x$n_final), "**."))
    if (!is.null(x$expected_diseased)) lines <- c(lines,
      paste0("- Expected disease-positive count in the complete population cohort: ", d(x$expected_diseased), "."),
      paste0("- Expected disease-negative count in the complete population cohort: ", d(x$expected_nondiseased), "."),
      "Expected counts are not guaranteed integer quotas. A prevalence-based cohort can miss the minimum positive or negative quota by chance.")
  }
  if(!is.null(x$strata)) {''')
save(p,s)

# Reusable design/FPC appendix, before Interpretation, in all ordinary reports.
p="R/reports_extended.R"; s=edit(p)
needle='calculation_markdown <- function(x) {'
appendix=r'''design_adjustment_markdown <- function(x) {
  using_design <- !is.null(x$design_source) && !identical(x$design_source, "none")
  using_fpc <- !is.null(x$population_size)
  if (!using_design && !using_fpc) return("")
  f <- formula_lines
  d <- display_number
  lines <- c("## Optional sampling adjustments", "")
  if (using_design) {
    lines <- c(lines, "### Design effect", "")
    if (identical(x$design_source, "icc")) {
      lines <- c(lines,
        f("DEFF=1+(M-1)\\rho"),
        f(paste0("DEFF=1+(", d(x$cluster_size), "-1)\\times", d(x$icc),
                 "=", d(x$design_effect))),
        "Here $M$ denotes mean cluster size and $\\rho$ denotes the intracluster correlation coefficient. The formula assumes approximately equal cluster sizes.")
    } else {
      lines <- c(lines, "The design effect was entered directly.",
        f(paste0("DEFF=", d(x$design_effect))))
    }
    if (inherits(x, "two_group_result")) {
      lines <- c(lines,
        if (x$mode == "sample_size") c(
          f("n_{1,\\mathrm{DEFF}}=DEFF\\times n_{1,\\mathrm{independent}}"),
          f(paste0("n_{1,\\mathrm{DEFF}}=", d(x$design_effect),
                   "\\times", d(x$n10_srs), "\\approx", d(x$n10)))) else c(
          f("SE_{\\mathrm{DEFF}}=\\sqrt{DEFF}\\times SE_{\\mathrm{independent}}"),
          "For fixed group sizes, the design effect increases the standard errors used in the approximate power calculation."))
      lines <- c(lines, "This variance-inflation approximation does not replace cluster-count or cluster-size-variation planning.")
    } else {
      lines <- c(lines,
        f("n_{\\mathrm{design}}=DEFF\\times n_{\\mathrm{SRS}}"),
        f(paste0("n_{\\mathrm{design}}=", d(x$design_effect), "\\times",
                 d(x$n_srs), "\\approx", d(x$n_design))))
    }
  }
  if (using_fpc) {
    reference_n <- if (using_design) "n_{\\mathrm{design}}" else "n_{\\mathrm{SRS}}"
    lines <- c(lines, "### Finite population correction", "",
      paste0("Finite sampling frame: $N=", d(x$population_size), "$."),
      f(paste0("n_{\\mathrm{FPC}}=\\frac{N\\times", reference_n,
               "}{N-1+", reference_n, "}")),
      f(paste0("n_{\\mathrm{FPC}}=\\frac{", d(x$population_size),
               "\\times", d(if (using_design) x$n_design else x$n_srs),
               "}{", d(x$population_size), "-1+",
               d(if (using_design) x$n_design else x$n_srs),
               "}\\approx", d(x$n0))),
      "This correction assumes sampling without replacement from a known finite eligible population; it is not automatically appropriate for randomized trials.")
  }
  if (!inherits(x, "two_group_result")) lines <- c(lines, "",
    f(paste0("n_{\\mathrm{complete}}=", d(x$n_complete))),
    f(paste0("n_{\\mathrm{final}}=", d(x$n_final))))
  paste(lines, collapse="\n")
}

'''
s=change(p,s,needle,appendix+needle)
s=change(p,s, '  if (exists("plot_appendix", mode = "function")) text <- paste(c(text, plot_appendix(x)), collapse = "\\n")', '''  adjustment <- design_adjustment_markdown(x)
  if (nzchar(adjustment)) text <- sub("## Interpretation", paste0(adjustment, "\\n\\n## Interpretation"), text, fixed = TRUE)
  if (exists("plot_appendix", mode = "function")) text <- paste(c(text, plot_appendix(x)), collapse = "\\n")''')
# Report direct-difference input without attributing artificial group means to the study.
s=change(p,s, '''    c(legend_row("\\\\mu_1", "Expected group 1 mean", d(x$mean1)),
      legend_row("\\\\mu_2", "Expected group 2 mean", d(x$mean2)),''', '''    c(if (identical(x$mean_input, "difference"))
        legend_row("D", "Entered signed difference: group 2 minus group 1", d(x$difference)) else
        c(legend_row("\\\\mu_1", "Expected group 1 mean", d(x$mean1)),
          legend_row("\\\\mu_2", "Expected group 2 mean", d(x$mean2))),''')
s=change(p,s, '''  delta_step <- f(if (proportions) paste0("\\\\delta=\\\\operatorname{abs}(", d(x$p2), "-", d(x$p1), ")\\\\approx", d(x$delta)) else
    paste0("\\\\delta=\\\\operatorname{abs}((", d(x$mean2), ")-(", d(x$mean1), "))\\\\approx", d(x$delta)))''', '''  delta_step <- f(if (proportions) paste0("\\\\delta=\\\\operatorname{abs}(", d(x$p2), "-", d(x$p1), ")\\\\approx", d(x$delta)) else
    if (identical(x$mean_input, "difference"))
      paste0("\\\\delta=\\\\operatorname{abs}(", d(x$difference), ")\\\\approx", d(x$delta)) else
      paste0("\\\\delta=\\\\operatorname{abs}((", d(x$mean2), ")-(", d(x$mean1), "))\\\\approx", d(x$delta)))''')
save(p,s)

p="R/reports_objectives.R"; s=edit(p)
s=change(p,s, '''  difference_numeric <- if (proportions) paste0("D=", d(x$p2), "-", d(x$p1), "\\\\approx", d(x$difference)) else
    paste0("D=(", d(x$mean2), ")-(", d(x$mean1), ")\\\\approx", d(x$difference))''', '''  difference_numeric <- if (proportions) paste0("D=", d(x$p2), "-", d(x$p1), "\\\\approx", d(x$difference)) else
    if (identical(x$mean_input, "difference")) paste0("D=", d(x$difference), "\\\\quad\\\\text{(entered directly)}") else
    paste0("D=(", d(x$mean2), ")-(", d(x$mean1), ")\\\\approx", d(x$difference))''')
s=change(p,s, '''    legend_row("\\\\mu_1", "Expected group 1 mean", d(x$mean1)), legend_row("\\\\mu_2", "Expected group 2 mean", d(x$mean2)),''', '''    if (identical(x$mean_input, "difference")) legend_row("D", "Entered signed mean difference", d(x$difference)) else
      c(legend_row("\\\\mu_1", "Expected group 1 mean", d(x$mean1)), legend_row("\\\\mu_2", "Expected group 2 mean", d(x$mean2))),''')
save(p,s)

# Amend inaccurate assumptions in published output.
p="R/report.R"; s=edit(p)
# The independent-observation numerator is shown separately from adjusted n0.
s=change(p,s, 'formula("n_0 = \\\\frac{Z_{1-\\\\alpha/2}^{2}\\\\,p(1-p)}{d^{2}}"),',
  'formula("n_{\\\\mathrm{SRS}} = \\\\frac{Z_{1-\\\\alpha/2}^{2}\\\\,p(1-p)}{d^{2}}"),')
s=change(p,s, 'paste0("| $n_0$ | Unrounded base number of complete observations | ", v$n0, " |"),',
  'paste0("| $n_0$ | Unrounded complete target after design and finite-frame adjustments | ", v$n0, " |"),')
s=change(p,s, 'formula(paste0("n_0\\\\approx\\\\frac{(", v$z, ")^{2}\\\\times", v$p,',
  'formula(paste0("n_{\\\\mathrm{SRS}}\\\\approx\\\\frac{(", v$z, ")^{2}\\\\times", v$p,')
s=change(p,s, 'formula(paste0("n_0\\\\approx ", v$n0)),',
  'formula(paste0("n_{\\\\mathrm{SRS}}\\\\approx ", display_number(x$n_srs))),')
s=change(p,s,'"- The population is treated as large relative to the sample; no finite population correction is applied.",', '''if (is.null(x$population_size)) "- No finite population correction was selected." else
      "- Finite-population correction assumes sampling without replacement from the specified finite eligible frame.",''')
s=change(p,s,'"- No cluster design effect or correction for measurement error is applied.",', '''if (x$design_effect == 1) "- No variance inflation was selected." else
      "- The entered design effect approximates within-cluster dependence. Check cluster counts and actual cluster-size variability.",''')
s=change(p,s,'"It uses the normal approximation with independent observations and optional non-response adjustment.", "",', '''"It uses the normal approximation with a configurable design effect, optional finite population correction and non-response adjustment.", "",''')
save(p,s)
p="R/reports_extended.R"; s=MODIFIED[p]
# Keep numerical substitutions for the iid variance distinct from DEFF inflation.
s=change(p,s, 'f("n_0=\\\\frac{Z_{1-\\\\alpha/2}^2\\\\sigma^2}{d^2}"),',
  'f("n_{\\\\mathrm{SRS}}=\\\\frac{Z_{1-\\\\alpha/2}^2\\\\sigma^2}{d^2}"),')
s=change(p,s, 'legend_row("n_0", "Unrounded complete sample size", d(x$n0)),',
  'legend_row("n_0", "Unrounded complete sample size after optional adjustments", d(x$n0)),')
s=change(p,s, 'f(paste0("n_0\\\\approx\\\\frac{(", d(x$z), ")^2\\\\times(", d(x$sd), ")^2}{(", d(x$d), ")^2}\\\\approx", d(x$n0))),',
  'f(paste0("n_{\\\\mathrm{SRS}}\\\\approx\\\\frac{(", d(x$z), ")^2\\\\times(", d(x$sd), ")^2}{(", d(x$d), ")^2}\\\\approx", d(x$n_srs))),')
s=change(p,s,'"- Observations are independent and the population is large relative to the sample.",', '''"- The variance calculation assumes the declared independence or design-effect approximation. A known finite sampling frame may be used for sampling without replacement.",''')
s=change(p,s,'"- No finite population correction, cluster effect, or bias correction is applied.",', '''"- Finite-population correction and design effect are optional; neither removes selection or measurement bias.",''')
s=change(p,s,'"- Groups and observations are independent. This method does not cover paired data or cluster designs.",', '''"- Groups are independent. An optional design-effect variance inflation approximates intracluster correlation; detailed cluster-trial designs require additional checks.",''')
save(p,s)
p="R/reports_objectives.R"; s=MODIFIED[p]
s=change(p,s, '"- Groups and observations are independent. Paired data, clusters, and finite population corrections require other methods.",', '''"- Groups are independent. A design-effect multiplier may approximate equal-size clusters; paired designs and finite-population corrections for trials are not supported.",''')
save(p,s)

p="README.md"; s=edit(p)
s=s.replace('The Yamane formula is a simplified survey-proportion method', '''Single-proportion and single-mean calculations provide optional **Apply design effect?** and **Apply finite population correction?** switches, both set to **No** by default. When design effect is enabled, enter a DEFF directly or estimate it from **DEFF = 1 + (M − 1) × ICC**. Both also offer optional finite-population correction for sampling without replacement from a known eligible population. Two independent proportions and two independent means support the same design-effect inputs for sample-size or fixed-size power planning; trial FPC is intentionally not offered. Mean comparisons allow a signed group-2-minus-group-1 difference directly, while retaining separate, reported pooled, and reference-computed SD inputs. These are first-order variance-inflation approximations and do not substitute for cluster-count or unequal-cluster-size planning.\n\nThe ROC AUC calculator defaults to the Cleveland Clinic RiskCalc single-AUC logit method, using anticipated AUC, null AUC 0.5, prevalence, two-sided alpha, and target power. It reports minimum diseased and non-diseased cases, total complete observations, and recruitment; see [the public source formula](https://github.com/ClevelandClinicQHS/riskcalc-website/blob/main/samplesize/samplesize.R). Existing Hanley–McNeil variance planning remains selectable for single-AUC testing and remains in place for estimation, paired and independent comparisons.\n\nThe Yamane formula is a simplified survey-proportion method''',1)
s=s.replace("AUC planning uses Hanley-McNeil variance and a normal approximation.",
  "Single-AUC hypothesis testing defaults to the RiskCalc/Lu logit-AUC normal approximation; Hanley-McNeil remains selectable. AUC precision and paired/independent comparisons retain Hanley-McNeil variance.")
s=s.replace("Paired data, clusters, exact binomial methods, and exact t or Welch methods require other calculations.",
  "Paired data, detailed cluster-number planning, exact binomial methods, and exact t or Welch methods require other calculations. Optional equal-cluster-size design effects are supported for the four standard estimation and group-comparison methods.")
save(p,s)

p="tests/testthat/test-design-adjustments.R"; s='''testthat::test_that("Design effect and finite population calculations have documented arithmetic", {
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
'''
save(p,s)

# Run all checks before writing any file.
if __name__ == '__main__':
    for filename,content in MODIFIED.items():
        if not isinstance(content,str) or not content.strip():
            raise RuntimeError(f'Empty patch result for {filename}')
    if args.check:
        print('Validated',len(MODIFIED),'updated source files; no files written.')
    else:
        for filename,content in MODIFIED.items():
            dest=ROOT / filename
            dest.parent.mkdir(parents=True,exist_ok=True)
            dest.write_text(content,encoding='utf-8')
            print('Updated:',filename)
        print('Next in R: source("scripts/run_tests.R")')
