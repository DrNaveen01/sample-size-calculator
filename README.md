# Sample Size Calculator

A reproducible sample-size calculator for health and medical research.

The project aims to provide statistically transparent sample-size calculations with:

- the statistical formula;
- substituted numerical values;
- intermediate calculation results;
- the final rounded sample size;
- a plain-language interpretation; and
- outputs suitable for research protocols and reports.

## Current development milestone

### v0.1.0 — Single-proportion sample-size calculator

The first prerelease will implement sample-size estimation for a single population proportion.

The calculation will use:

\[
n_0 = \frac{Z_{1-\alpha/2}^{2}p(1-p)}{d^2}
\]

where:

- \(n_0\) is the initial required sample size;
- \(p\) is the expected population proportion;
- \(d\) is the required absolute precision;
- \(\alpha\) is the significance level; and
- \(Z_{1-\alpha/2}\) is the corresponding standard-normal critical value.

When a non-response proportion \(r\) is specified:

\[
n_{\text{final}} =
\left\lceil
\frac{n_0}{1-r}
\right\rceil
\]

## Planned v0.1.0 features

- Single-population-proportion calculation
- Confidence-level input
- Expected-proportion input
- Absolute-precision input
- Optional non-response adjustment
- Input validation
- Formula display
- Numerical substitution
- Upward rounding of the final sample size
- Plain-language interpretation
- Markdown output
- HTML output
- Automated tests
- Worked examples

## Project structure

```text
R/                  R calculation and output functions
tests/testthat/      Automated unit tests
examples/            Reproducible worked examples
docs/                Project and statistical documentation
.github/workflows/   GitHub Actions workflows