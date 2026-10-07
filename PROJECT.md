# Project development plan

## Objective

Develop a transparent and reproducible sample-size calculator for health and medical research.

## Design principles

1. Separate statistical calculations from formatting.
2. Return structured results.
3. Retain full precision and round the final sample size upward.
4. Verify methods against independently computed examples.
5. State assumptions and limitations explicitly.
6. Use a common engine and report for HTML, Markdown, PDF, DOCX, and Shiny.

## Current scope

The first implementation covers estimation of one population proportion. On 5 October 2026, the user extended the scope to include Shiny and downloadable PDF and Word reports.

### Included

- Absolute and relative precision
- Custom confidence or direct Z input
- Non-response adjustment using the unrounded base
- Input validation and structured results
- Generic formulas, legends, substitution, intermediate values, final rounding, and interpretation
- Assumptions and references
- Live Shiny interface with copyable Markdown
- Markdown, HTML, PDF, and DOCX exports
- Calculation, export, and server checks
- Worked documentation and automated tests

### Deferred

- Finite population correction
- Cluster design effects
- Two-proportion calculations with OR or RR conversions
- Mean-based calculations
- 2 by 2 effect-measure calculator

## Definition of done

The implementation is ready for review when the engine and app are complete, independently computed examples and exports pass verification, reports include all requested calculation detail, and setup and deployment are documented.

A release is complete after review and merge, successful automated checks, closure of the implementation issue, and publication of the agreed release. An implementation branch or pull request does not publish a release or deploy a hosted Shiny app.
