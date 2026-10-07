# Implementation verification

Verified on 5 October 2026.

The calculation, report, export, and Shiny server tests passed. Browser checks also passed for live input changes, absolute and relative precision, invalid-input handling, reset, copying from both Markdown buttons, and actual Markdown, PDF, and Word downloads. Desktop and mobile layouts were inspected, with no horizontal page overflow in the checked viewports.

## Calculation checks

The expected results were computed independently from tabulated normal quantiles using SciPy and compared with the R implementation. Additional checks confirm that halving the absolute margin of error multiplies the unrounded sample size by four, complementary proportions give the same result, and non-response inflation uses the unrounded base value.

| Expected proportion | Absolute precision | Confidence | Non-response | Complete observations | Recruitment target |
|---:|---:|---:|---:|---:|---:|
| 0.50 | 0.05 | 95% | 0% | 385 | 385 |
| 0.50 | 0.05 | 95% | 10% | 385 | 427 |
| 0.33 | 0.05 | 95% | 0% | 340 | 340 |
| 0.20 | 0.02 | 95% | 10% | 1537 | 1708 |
| 0.50 | 0.05 | 99% | 0% | 664 | 664 |

## Report checks

The PDF and Word example reports were rendered and every page inspected. Equations, legends, substituted values, results, assumptions, references, and page numbers are present. Word equations are native OMML. Word uses the equivalent ceil function notation to avoid ceiling-bracket rendering failures in some readers. The example PDF has two pages; the Word report groups setup, calculation, and interpretation into three pages.

The copied Markdown was checked against the content of the actual downloaded Markdown file. PDF and Word downloads were verified as valid file formats. Export tests confirm that the Word document contains native equations and the final recruitment target.

## Reproduce checks

```r
source("scripts/run_tests.R")
source("examples/worked_example.R")
```

The checked environment used R 4.3.3, Shiny 1.8.0, rmarkdown 2.25, testthat 3.2.1, Pandoc, and a working LaTeX installation. GitHub Actions checks are supplied but were not executed remotely because the integration rejected branch creation with HTTP 403. No hosted Shiny deployment was performed.

References: [Shiny testing](https://shiny.posit.co/r/articles/improve/server-function-testing/), [R Markdown and Pandoc conversion](https://rmarkdown.rstudio.com/docs/reference/pandoc_convert.html).
