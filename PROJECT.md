# Project development plan

## Objective

Develop transparent and reproducible sample-size calculators for research, with a Shiny interface and reports that show generic formulas and numerical substitutions.

## Current scope

- Single proportion: absolute or relative precision, confidence or supplied Z
- Single mean: anticipated SD and absolute precision, confidence or supplied Z
- Two independent proportions: direct probabilities or baseline plus a group 2 / group 1 OR or RR
- Two independent means: separate SDs, directly reported pooled SD, or pooled SD computed from reference SDs and group sizes
- Two-group objectives: equality (two-sided difference test), directional superiority, non-inferiority, and equivalence
- Direction of benefit, superiority or non-inferiority margin, and lower/upper equivalence margins where applicable
- Target power and unequal allocation for sample size, or complete group sizes for approximate power
- Editable group names in input labels, results, formula legends, and all reports
- Taro Yamane simplified finite-population survey formula with explicit assumptions
- Non-response inflation of unrounded sizes, followed by separate upward rounding of complete and recruitment targets
- Power results displayed to at most two decimal places; full precision retained internally
- Independent scrolling of input and results columns on desktop; stacked scrolling on mobile
- Shared reports with generic formulas, legends, substituted values, interpretation, assumptions, and references
- Copy Markdown, save Markdown, and download PDF and DOCX
- Independent numerical fixtures, calculation, server, report, export, and browser verification

## Design principles

Keep calculations separate from reports. Use one report source across outputs. Show complete and recruitment targets separately. Explain ratio direction, named groups, the selected hypothesis, alpha, and clinical margins. Validate active inputs. Verify numerical calculations against independent references and inspect rendered documents.

The equality option tests a zero difference; it does not demonstrate identical groups. Non-significance in that test does not establish equivalence or non-inferiority. Comparison objectives follow [ICH E9, sections 3.3.2 and 3.5](https://www.ema.europa.eu/en/documents/scientific-guideline/ich-e-9-statistical-principles-clinical-trials-step-5_en.pdf).

## Next update

AUC estimation, sensitivity, specificity, and diagnostic-accuracy sample-size calculations. These are recorded for later implementation. Define estimation precision versus hypothesis testing and the required diseased/non-diseased counts before selecting methods.

## Other deferred methods

Cluster designs, paired designs, exact t or Welch planning, continuity corrections, exact binomial methods, general finite population corrections for the other calculators, and the separate 2 by 2 effect-measure calculator.

## Definition of done

The implementation is ready for review after numerical, report, export, and interface checks pass and setup and methods are documented. A branch or pull request does not publish a release or deploy a hosted Shiny app.
