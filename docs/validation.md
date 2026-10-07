# Implementation verification

Verified on 8 October 2026.

Engine, report, export, and Shiny server tests passed for all five calculators. Both two-group calculators support equality, superiority, non-inferiority, and equivalence in sample-size and fixed-size power modes. The original single-proportion API and source-loading sequence remain supported.

## Independent calculation checks

Equality calculations were checked against statsmodels 0.14.6: pooled-null and unpooled-alternative variance for proportions, and specified-variance normal power for means. The existing fixture covers equal and unequal allocation, different SDs, multiple power targets, and significance levels. A published PASS example (.54 versus .44, 90% power, two-sided 5%) reproduces 524 complete observations per group and approximately 90.05% power.

Seven additional fixtures cover directional and equivalence objectives for both outcomes. Directional results use independent statsmodels normal-power checks. Equivalence results integrate a standardized normal density over the joint rejection interval and solve the target-power equation with SciPy's root finder. This checks joint power without reusing the R engine's CDF calculation. Fixtures compare unrounded first-group size, both upward-rounded counts, and power at the complete counts. Symmetric equivalence also matches its analytic special case.

| Example | Complete target | Recruitment target with 10% non-response |
|:---|:---|:---|
| Single proportion: p=.50, d=.05, 95% confidence | 385 | 427 |
| Single mean: SD=10, margin=2, 95% confidence | 97 | 107 |
| Two proportions, equality: .20 versus .30, 80% power, two-sided alpha=.05, k=2 | 224 and 447 | 249 and 497; total 746 |
| Two means, equality: 100 versus 105, SDs 15 and 20, 80% power, two-sided alpha=.05, k=2 | 134 and 267 | 149 and 297; total 446 |
| Two proportions, superiority: .20 versus .30, margin=0, 80% power, one-sided alpha=.025, k=2 | 208 and 416 | 232 and 463; total 695 |
| Two means, non-inferiority: means 100 and 100, margin=5, reference SDs 15 and 20 with counts 50 and 100, 80% power, alpha=.025, k=2 | 162 and 323 | 179 and 358; total 537 |
| Two proportions, equivalence: .20 versus .21, margins -.05 and .04, 80% joint power, alpha=.05 per test, k=2 | 1,672 and 3,343 | 1,857 and 3,714; total 5,571 |
| Yamane: population=1,000, precision=.05 | 286 | 318 |

Here k is the second-group / first-group allocation ratio. The reference-weighted pooled SD in the non-inferiority example is 18.494886, with approximate power 80.18% at complete counts. The asymmetric equivalence example has approximate power 80.02%. With fixed complete counts 100 and 200, the equality proportion example has power 45.25% and the unequal-SD mean example has power 67.92%.

Other checks cover direction reversal, reciprocal allocation, zero superiority margin, positive non-inferiority margins, expected differences outside equivalence limits, zero joint rejection probability, invalid alpha, proportional margins, whole-number counts, separate SDs, direct pooled SD, degrees-of-freedom weighting, inactive input validation, OR/RR conversion, units, rounding, and non-response inflation. Yamane checks include invalid population sizes and a warning when the recruitment target exceeds the population. Custom names are validated and escaped safely in reports.

Calculation precision is retained internally. Final power percentages have at most two decimal places. Reports use n_final notation and explain upward rounding once, rather than repeatedly showing rounding operators. For equal mean SDs 15 and a difference of 5, 10% non-response gives 157 recruits per group; inflating the already-rounded complete count would incorrectly give 158 under this app's convention.

## Interface and export checks

A real Chromium browser exercised all five calculators, all comparison objectives, power and sample-size modes, direction controls, unequal allocation, separate/direct/reference SD modes, names, OR/RR inputs, invalid-input clearing, and reset. On desktop, wheel scrolling one column left the other column and document position unchanged. Desktop (1440 by 1050) and mobile (390 by 844) layouts had no horizontal page overflow.

Both copy buttons and actual Markdown, PDF, and Word downloads were checked. Clipboard text exactly matched downloaded Markdown. Power displays followed the two-decimal maximum. Custom names appeared in controls, result cards, legends, and exported reports. Native MathML was present, including proper pooled-SD headings.

All fourteen worked scenarios were generated as Markdown and HTML. Seven representative PDF/Word pairs were rendered and inspected page by page: single proportion, single mean, reported pooled SD, reference-pooled non-inferiority, proportion superiority, proportion equivalence, and Yamane. Generic formulas, legends, substituted values, intermediate calculations, interpretation, assumptions, references, and page numbers were reviewed. Word equations use native OMML. Word comparisons flow naturally across pages, and PDF legend headings stay with their table headers and first rows.

## Reproduce checks

To run the R checks and regenerate worked reports, source scripts/run_tests.R and examples/all_calculators.R from the project root.

For independent Python fixtures, install statsmodels 0.14.6 and SciPy in a verification environment, then run scripts/validate_independently.py and scripts/validate_objectives.py from the project root. Python is not an app dependency.

The checked runtime used R 4.3.3, Shiny 1.8.0, rmarkdown 2.25, testthat 3.2.1, Pandoc 3.1.3, and TeX Live. The GitHub Actions workflow runs application tests and generates reports; the project is a Shiny application rather than an R package.

The current implementation was verified locally. GitHub branch creation returned HTTP 403, "Resource not accessible by integration"; this extension has not been submitted or verified remotely. No hosted Shiny deployment was performed. AUC, sensitivity, specificity, and diagnostic accuracy remain on the later-update roadmap.
