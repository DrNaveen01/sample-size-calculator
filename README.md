# Sample Size Calculator

A reproducible R and Shiny calculator for estimating a single population proportion.

## Run the app

Open sample-size-calculator.Rproj in RStudio. From the project root:

```r
source("scripts/setup.R")
shiny::runApp(".")
```

RStudio supplies Pandoc. PDF export also needs TeX Live, MiKTeX, or TinyTeX with pdflatex on PATH. The setup script reuses an existing installation. To install TinyTeX when no LaTeX installation exists:

```r
Sys.setenv(INSTALL_TINYTEX = "true")
source("scripts/setup.R")
```

## Included

- Absolute precision in percentage points or relative precision as a percentage of the expected proportion
- Custom two-sided confidence level or directly supplied Z value
- Optional non-response adjustment, complete-observation target, and final recruitment target
- Live results, input validation, and input-specific checks
- Generic formulas, legends, numerical substitution, intermediate results, final rounding, interpretation, assumptions, and references
- Copy as Markdown and downloadable Markdown, PDF, and Word reports
- Editable Word equations and local MathML in the browser preview
- Shared calculation and report content across every output
- Numerical, export, and Shiny server tests and GitHub Actions checks

The scope now includes the requested Shiny interface and document exports. Two-proportion and 2 by 2 effect-measure calculators remain separate future work.

## Reproduce a calculation

The calculation function accepts proportions rather than percentages.

```r
source("R/single_proportion.R")
source("R/report.R")
source("R/exports.R")

result <- single_proportion(
  p = 0.50, precision = 0.05,
  confidence = 0.95, nonresponse = 0.10
)
print(result)
# Complete observations required: 385
# Participants to approach: 427

single_proportion_export(result, "calculation.md", "markdown")
single_proportion_export(result, "calculation.pdf", "pdf")
single_proportion_export(result, "calculation.docx", "docx")

# Generate all worked-example outputs and run checks:
source("examples/worked_example.R")
source("scripts/run_tests.R")
```

Calculations use the normal approximation for a large population with independent observations. The engine keeps full precision and applies non-response inflation to the unrounded base value. It rounds the final recruitment target upward once. This method plans estimation precision rather than hypothesis-test power.

## Project structure

| Path | Purpose |
|:---|:---|
| app.R | Shiny entry point |
| R/single_proportion.R | Calculation and validation |
| R/report.R | Shared report content |
| R/exports.R | HTML, Markdown, PDF, and DOCX conversion |
| R/app_ui.R and R/app_server.R | Interface and reactive behavior |
| www/ | Responsive styles and clipboard behavior |
| templates/ | Word reference and title styles |
| scripts/ and examples/ | Setup, checks, and reproducible example |
| tests/testthat/ | Calculation, report, export, and server checks |
| docs/ | Method and deployment instructions |

See [methods](docs/methods.md) for formulas and assumptions and [deployment](docs/deployment.md) for hosting requirements. An R-capable Shiny host is required; GitHub Pages cannot execute this app.

## References

1. Lwanga SK, Lemeshow S. *Sample size determination in health studies a practical manual*. WHO; 1991. https://iris.who.int/handle/10665/40062
2. Penn State Department of Statistics. *STAT 500 Confidence intervals*. https://online.stat.psu.edu/stat500/Lesson05
3. Posit. *Shiny file downloads*. https://shiny.posit.co/r/reference/shiny/latest/downloadhandler.html
4. Posit. *Convert a document with Pandoc*. https://rmarkdown.rstudio.com/docs/reference/pandoc_convert.html

MIT licence. Copyright 2026 Dr Naveen Suthar.
