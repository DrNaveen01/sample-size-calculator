# Changelog

## 0.7.0 review candidate 9 October 2026

- Added AUC precision, arbitrary-null AUC tests, independent and paired AUC comparisons, complete disease quotas, and covariance sensitivity.
- Added sensitivity, specificity, joint precision, Wilson/Wald methods, prevalence planning, and normal-score benchmark testing.
- Added Pearson correlation precision and testing, non-zero nulls, independent comparisons, and fixed-size power.
- Added design-aware 2 by 2 effects, explicit zero-cell handling, Newcombe risk-difference intervals and unbounded NNT/NNH confidence sets.
- Added trial hypothesis, power, expected-effect and margin plots; portable plot embedding and PNG/CSV downloads.
- Preserved existing engines and APIs, numeric substitutions, scrolling, custom names, pooled SD inputs and report templates.
- Rebuilt the lockfile from the tested environment, reproduced the previous dependency parser error, and verified local deployment manifest generation.
- Consolidated duplicate checks and made deployment manual with prerequisite checks.
- Added independent numerical fixtures, reactive validation and native equation/image export checks, methods and a feature matrix.

## Previous implementation

- Added equality, directional superiority, non-inferiority, and joint two-one-sided-test equivalence objectives for both two-group calculators.
- Added direct pooled SD input and reference-weighted pooled SD computation with formulas and numerical substitution.
- Added editable group names across input labels, results, formula legends, and exports.
- Added the Taro Yamane finite-population calculator with its fixed confidence and proportion assumptions.
- Limited displayed power results to two decimal places and replaced repeated rounding-operator expressions with final sample-size notation.
- Added independent desktop scrolling for input and output columns.
- Originally recorded diagnostic methods for the next update; these are now included in 0.7.0.
- Added independent density-integration reference fixtures for the new objectives.
- Improved Word pagination and kept PDF legend headings with their tables.

- Added a single-mean precision calculator and two independent proportion and mean comparison calculators.
- Added target power, two-sided significance, and unequal group allocation controls, with complete and recruitment sizes per group and total.
- Added approximate power checking from fixed complete group sizes for both comparisons.
- Added baseline plus odds-ratio or risk-ratio input for two proportions and separate anticipated standard deviations for two means.
- Extended the shared formula, legend, numerical-substitution, Markdown, PDF, and Word reports to all calculators and power modes.
- Added independent statsmodels reference fixtures, meaningful calculation, server and export checks, and worked examples for every method.
- Preserved single-proportion estimation with absolute or relative precision, supplied Z, confidence, and non-response inflation.

## Initial single proportion implementation

- Responsive Shiny interface with live results and validated inputs.
- Copyable Markdown and downloadable Markdown, PDF and DOCX.
- Shared detailed reports, native Word equations and local MathML previews.
- Calculation, report and server checks with GitHub Actions verification.
