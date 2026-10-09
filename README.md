<!-- badges: start -->
[![Calculator checks](https://github.com/DrNaveen01/sample-size-calculator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DrNaveen01/sample-size-calculator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Sample Size Calculator

An R and Shiny app for sample-size planning and approximate power for independent observations. Every calculation can be copied as Markdown or downloaded as PDF or Word, with generic formulas, legends, numerical substitution, intermediate results, interpretation, and references.

## Run the app

Open sample-size-calculator.Rproj in RStudio. From the project root:

```r
source("scripts/setup.R")
shiny::runApp(".")
```

RStudio supplies Pandoc. PDF export also needs TeX Live, MiKTeX, or TinyTeX with pdflatex on PATH. The setup script reuses an existing installation. To install TinyTeX if no LaTeX installation exists:

```r
Sys.setenv(INSTALL_TINYTEX = "true")
source("scripts/setup.R")
```

## Calculators

| Calculator | Planning inputs | Results |
|:---|:---|:---|
| Single proportion | Expected proportion, absolute or relative margin, confidence or custom Z | Complete observations and recruitment target |
| Single mean | Standard deviation, absolute margin in the same units, confidence or custom Z | Complete observations and recruitment target |
| Two proportions | Two expected proportions, or group 1 proportion plus OR or RR; significance level, target power, allocation ratio | Complete and recruitment sizes for each group and total |
| Two means | Two expected means; separate SDs, reported pooled SD, or reference SDs and group sizes; significance, power, allocation | Complete and recruitment sizes for each group and total |
| Taro Yamane | Finite population size and precision; optional non-response | Complete observations and recruitment target |

For comparisons, select **Sample size** to enter target power and the group size ratio, or **Power** to enter complete group sizes and calculate approximate power. The allocation ratio is **group 2 / group 1**: 1 gives equal groups; 2 plans twice as many complete observations in group 2. OR and RR also refer to group 2 relative to group 1. The same event must define both proportions. Different group standard deviations are allowed for two means.

For both comparisons, choose **Equality**, **Superiority**, **Non-inferiority**, or **Equivalence**. Equality is a two-sided test of a zero difference. Superiority and non-inferiority use a declared direction of benefit and a one-sided alpha. Equivalence uses lower and upper margins for the signed second-group minus first-group difference, with alpha specified for each of the two one-sided tests. The help text identifies the alpha convention. Margins must be clinically justified; a non-significant equality test does not establish equivalence.

Group names are editable and follow the inputs, results, formula legends, and reports. In two means, pooled-SD modes assume a common population variance. Reference group sizes weight the within-group pooled SD and remain separate from the planned allocation. The report shows the SD formula and numerical calculation.

Target power for sample-size mode must be above 50% and below 100%. Power mode takes whole-number complete, analysable sizes of at least two per group; no non-response inflation is applied. Displayed power has at most two decimal places; computation retains full precision. On desktop, the input and output columns scroll independently. Mobile uses a stacked layout.

The Yamane formula is a simplified survey-proportion method for a known finite population. It assumes simple random sampling, approximately 95% confidence, and a proportion of 0.5. Its confidence and assumed proportion are fixed, and it has no power input. See [methods and references](docs/methods.md).

## Reports and rounding

- Live calculation preview with locally generated MathML
- Copy as Markdown, save .md, download PDF, and download Word with editable equations
- Generic formulas, every symbol explained, input values, substituted equations, intermediate results, and final rounding
- Per-group sizes, approximate power checks, assumptions, references, and input-specific warnings
- A single report source for the interface, clipboard, and all downloaded formats

The engine retains full precision. In sample-size mode, non-response inflation is applied to each unrounded base size; recruitment targets are then rounded upward separately. Rounded complete targets are shown separately. Rounding can slightly alter the requested allocation ratio. Non-response inflation compensates for expected loss of observations; it does not correct bias.

These methods assume independent observations and a large population. The mean calculations treat anticipated standard deviations as known for planning; they are not exact t-test or Welch power calculations. For equality, two-proportion planning uses pooled null variance and unpooled alternative variance. The directional and equivalence objectives use an unpooled Wald approximation. No continuity correction is applied. Paired data, clusters, exact binomial methods, and exact t or Welch methods require other calculations. Yamane is the only finite-population method currently included.

## Reproduce calculations

```r
for (file in c("single_proportion.R", "planning.R", "report.R",
               "reports_extended.R", "reports_objectives.R", "exports.R")) {
  source(file.path("R", file))
}

single_proportion(.50, .05, nonresponse = .10)
single_mean(sd = 10, precision = 2, nonresponse = .10)

# 80% target power, 5% two-sided significance, 2:1 allocation.
result <- two_proportions(.20, .30, power = .80, alpha = .05,
                          ratio = 2, nonresponse = .10)
print(result)
# Complete group sizes: 224 and 447
# Recruitment group sizes: 249 and 497; total 746
calculation_export(result, "calculation.md", "markdown")
calculation_export(result, "calculation.pdf", "pdf")
calculation_export(result, "calculation.docx", "docx")

# Two independent means with different standard deviations.
two_means(100, 105, sd1 = 15, sd2 = 20, power = .80, ratio = 2)

# Group 2 expected proportion derived from group 1 and RR or OR.
two_proportions(.20, effect_type = "rr", effect = 1.5)
two_proportions(.20, effect_type = "or", effect = 12/7)

# Approximate power for fixed complete group sizes.
two_proportions(.20, .30, n1 = 100, n2 = 200)
two_means(100, 105, sd1 = 15, sd2 = 20, n1 = 100, n2 = 200)

# Non-inferiority of two means, with pooled SD computed from a reference study.
two_means(100, 100, 15, 20, objective = "noninferiority", margin = 5,
          alpha = .025, sd_method = "reference", ref_n1 = 50, ref_n2 = 100,
          group1 = "Standard care", group2 = "New treatment")

# Enter a pooled SD directly.
two_means(100, 105, sd_method = "pooled", pooled_sd = 15)

# Joint equivalence power, asymmetric margins, unequal allocation.
two_proportions(.20, .21, objective = "equivalence", lower = -.05,
                upper = .04, alpha = .05, ratio = 2)

# Directional superiority; use direction = "lower" when lower is better.
two_means(100, 105, 15, objective = "superiority", alpha = .025)

# Taro Yamane: population 1000, 5 percentage-point precision, 10% non-response.
yamane(1000, precision = .05, nonresponse = .10)

source("scripts/run_tests.R")
source("examples/all_calculators.R")
```

The downloadable bundle includes Markdown and HTML examples for every scenario, and PDF/Word examples for all 14 original scenarios plus four representative new pairs covering paired AUC, joint diagnostics, correlation precision and manual effects. The example script regenerates all formats for every scenario.

The original `single_proportion()` and `single_proportion_export()` functions and their original source-loading sequence remain supported. New calculators and exports use the source-loading sequence above. R calculation APIs accept probabilities as fractions; the interface accepts percentages.

## Project structure

| Path | Purpose |
|:---|:---|
| app.R | Shiny entry point |
| R/single_proportion.R and R/planning.R | Calculation engines and validation |
| R/report.R, R/reports_extended.R, and R/reports_objectives.R | Shared report content |
| R/exports.R | Markdown, PDF, DOCX, and HTML conversion |
| R/app_ui.R and R/app_server.R | Interface and reactive behavior |
| www/ | Responsive styles and clipboard behavior |
| templates/ | Word reference, report styles, and PDF pagination |
| scripts/ and examples/ | Setup, verification, reproducible examples |
| tests/testthat/ and tests/fixtures/ | Engine, report, export, server, and independent reference checks |
| docs/ | Methods, validation, and deployment instructions |

See [methods](docs/methods.md), [validation](docs/validation.md), and [deployment](docs/deployment.md). An R-capable Shiny host is required; GitHub Pages cannot execute this app.

## Additional calculators and planning plots

The application also includes full ROC AUC estimation, single-AUC testing against an arbitrary null, independent and paired AUC comparisons, sensitivity/specificity/joint precision, accuracy benchmark tests, Pearson correlation estimation and testing, independent correlation comparison, and manual 2 by 2 effect measures.

AUC planning uses Hanley-McNeil variance and a normal approximation. Paired comparisons require the correlation between estimated AUCs; raw test-score correlation is not interchangeable. Diagnostic estimation offers Wilson expected intervals or Wald planning. Correlation uses an explicitly uncorrected Fisher-z approximation. Read [additional methods](docs/advanced-methods.md) before applying these calculators.

Two-group trial plots show hypothesis boundaries, approximate power, expected-effect sensitivity, and non-inferiority/equivalence margin sensitivity. New modules provide precision, prevalence, covariance, and correlation sensitivity plots where applicable. Download plots as PNG and plot data as CSV. The same report source embeds portable PNG plots in Markdown, HTML, PDF and Word. Self-contained Markdown files are larger because their figures use data URIs.

New modules solve for minimum whole-number complete quotas and then inflate those quotas for losses. Existing calculators retain their documented original rounding convention. Population disease recruitment targets are based on expected prevalence and do not guarantee realised quotas.

For the new R APIs:

```r
for (name in c("single_proportion", "planning", "report", "reports_extended",
               "reports_objectives", "exports", "advanced", "effects", "plots",
               "reports_advanced")) source(paste0("R/", name, ".R"))
auc_sample(auc = .80, objective = "estimate", precision = .05)
auc_sample(auc = .80, null_auc = .65, objective = "test")
auc_sample(auc = .80, auc2 = .75, objective = "paired", auc_correlation = .50)
diagnostic_sample(objective = "joint", prevalence = .20, nonresponse = .10)
correlation_sample(r = .50, r0 = .20)
correlation_sample(r = .30, objective = "estimate", precision = .10)
effect_table(20, 80, 40, 60, design = "trial")
source("examples/advanced_calculators.R")
```

Opening the project does not automatically download renv. To use the pinned environment, install renv and run `renv::restore(prompt = FALSE)`; set `CALCULATOR_USE_RENV=true` to activate its library automatically. The lockfile records the tested R 4.3.3 environment. Deployment is manual through GitHub Actions; ordinary pushes do not publish the app.

See [feature matrix](docs/feature-matrix.md), [validation](docs/validation.md), and [development plan](PROJECT.md).

MIT licence. Copyright 2026 Dr Naveen Suthar.
