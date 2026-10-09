# Validation summary

Verified locally on 9 October 2026 against repository base `8c0dbe326d5c6ce97e831ea4a96790fc8c498270`. This distinguishes numerical checks, automated behaviour, rendered inspection and checks that were not performed.

## Automated checks

The final full R run passed **556 expectations in 38 test cases**, with zero failures, warnings, errors or skips. It covers the preserved APIs, five original calculators, all four two-group objectives, fixed-complete-size power, allocation, SD modes, OR/RR conversions, active-input validation, server stale-result clearing, new engines, report content and actual PDF/Word generation. `output/test-results.csv` records the run. The subsequent correlation report notation refinement was checked separately and its examples regenerated.

## Independent numerical verification

Python verification regenerated 30 numerical fixtures: eight equality cases, seven directional/joint-equivalence cases, twelve additional planning cases and three effect-table cases. The fixture files did not change on regeneration. Verification scripts are independent of the R application. Their use validates implementation of the declared approximation, not its clinical adequacy for every design.

- Existing equality references use statsmodels 0.14.6 normal-power engines, including pooled-null/unpooled-alternative proportion variance and specified-variance mean power. The published PASS .54 versus .44 example gives 524 complete observations per group and about 90.05% power.
- Directional references use independent normal-power calculations. Equivalence integrates the standardized normal density over the **joint** rejection interval using SciPy and independently solves the sample-size equation. This does not substitute single-component power for joint power.
- AUC references independently implement the published Hanley–McNeil variance and integrate the normal rejection region with SciPy. Independent and paired variances are separate; paired calculations use explicit estimated-AUC correlation, not raw-score correlation or an invented covariance.
- Diagnostic Wilson/Wald anticipated intervals are checked against statsmodels; benchmark tests use independently integrated normal-score power. Quotas and prevalence-based recruitment maxima are checked independently.
- Pearson calculations use independently integrated, uncorrected Fisher-z normal power and back-transformed anticipated intervals. These are not exact Pearson t-test or bias-corrected `pwr.r.test` calculations.
- Effect-table ratios and confidence limits are checked against statsmodels Table2x2 and independently computed Newcombe Wilson limits. R `stats::fisher.test` supplies conditional odds ratio and exact interval when zero cells are left uncorrected.

Exact methods, approximation assumptions and references are in [methods](methods.md) and [additional methods](advanced-methods.md).

## Worked results

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


The original five calculators preserve inflation from unrounded sizes. The new modules first establish whole complete quotas, then inflate and round each recruitment quota. Both conventions retain full internal precision, report their convention, and sum per-group counts consistently. For the original equal-SD mean example, recruitment is 157 per group rather than the 158 obtained by first inflating the rounded complete quota.

| New example | Complete requirement | Recruitment target |
|:---|:---|:---|
| AUC .8, normal CI half-width .05, equal strata | 151 diseased + 151 non-diseased | 302 with no losses |
| Paired AUC .8 versus .75, estimated-AUC correlation .5, 80% power, two-sided .05 | 341 diseased + 341 non-diseased | 379 + 379 = 758 with 10% losses |
| Joint Wilson sensitivity .85/specificity .90, each half-width .05, prevalence .20 | 196 diseased, 141 non-diseased quotas; population cohort 980 | 1,089 with 10% losses; sensitivity limits recruitment |
| Pearson .30 versus zero, 80% power, two-sided .05 | 85 participant pairs | 85 with no losses |
| Pearson .30, correlation-scale interval half-width .10 | 320; anticipated interval .19683425 to .39659521 | 356 with 10% losses |
| 2×2 rows (20,80) and (40,60) | 200 observed participants | Not recruitment planning; OR .375, RR .5, RD −.2, NNT 5 |

## Browser and visual inspection

A real Chromium headless browser exercised all nine calculators and all four trial objectives. It checked invalid AUC covariance input clears results and Markdown, fixed-size AUC power, case-control risk-measure suppression, actual diagnostic Word download, mobile horizontal overflow and independent desktop column scrolling. There were no page JavaScript errors. At 1440×1050, input scrolling moved 500 pixels while output and document positions stayed fixed; at 390×844 there was no horizontal page overflow. Evidence and representative screenshots are in `output/browser/`. This is a smoke test, not a comprehensive accessibility or cross-browser audit.

All 14 original scenarios and 21 new scenarios were generated reproducibly as Markdown and HTML; original PDF/Word reports and four representative new PDF/Word pairs are included. The four new pairs (paired AUC, joint diagnostics, correlation precision and trial effect table) were rendered and inspected page by page. Native editable Word equations, numerical substitutions, tables, plots, references and page numbers were reviewed. Long reference URLs that overflowed PDF margins were replaced by descriptive working hyperlinks. The report pipeline and template were preserved. Images and equations in source alone were not used as visual evidence.

## Dependency and deployment verification

The original lock reproduced `rsconnect` dependency parsing failure (`subscript out of bounds`, unresolved `otel` dependency). A canonical regenerated lock contains the transitive dependency closure. Local `rsconnect` dependency parsing and manifest generation passed with **59 dependencies**. No shinyapps.io publication was performed; credentials are read only by the explicit deployment script/workflow and are not bundled.

GitHub branch creation returned HTTP 403, “Resource not accessible by integration.” The deliverable contains the complete local update. No remote update, GitHub Actions execution, merge, deployment or issue closure is claimed. The consolidated CI configuration is checked locally through its component commands; its hosted run remains unverified.

## Reproduce

Run from the project root after installing the documented R dependencies:

```sh
Rscript --vanilla scripts/run_tests.R
python scripts/validate_independently.py
python scripts/validate_objectives.py
python scripts/validate_advanced.py
Rscript --vanilla examples/all_calculators.R
Rscript --vanilla examples/advanced_calculators.R
Rscript --vanilla scripts/deployment_preflight.R
```

Python is a verification dependency, not an application dependency. Use SciPy and statsmodels 0.14.6. The checked environment used R 4.3.3, Shiny 1.8.0, rmarkdown 2.25, testthat 3.2.1, Pandoc and TeX Live. To repeat the browser smoke test, install Playwright in a verification environment, start `shiny::runApp(host="127.0.0.1", port=8766)` and run `node scripts/browser_smoke.cjs`; `APP_URL` and `CHROME_EXECUTABLE` can override its defaults.

## Unresolved scope and limitations

See the [feature matrix](feature-matrix.md). Spearman, dependent/repeated-measures correlations, partial AUC/pilot-data DeLong planning, exact diagnostic-binomial planning and exact t/Welch group planning remain explicitly deferred. Population prevalence planning supplies expected strata, not a probabilistic guarantee that quotas will be reached. Paired AUC planning requires a justified estimated-AUC correlation and provides sensitivity analysis. Joint diagnostic precision uses individual endpoint confidence levels, not simultaneous coverage. Normal approximations need design-specific assessment for small samples, extreme probabilities or uncertain covariance. Hosted deployment, remote CI and a clean isolated `renv::restore()` were not run.
