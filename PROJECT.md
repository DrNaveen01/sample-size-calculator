# Project development plan

## Objective

Develop a transparent and reproducible sample-size calculator for health and medical research.

## Design principles

1. Statistical calculations must be separated from output formatting.
2. Functions must return structured results rather than only printed text.
3. Intermediate values must not be rounded.
4. The final required sample size must always be rounded upward.
5. Every supported method must include independently verified test cases.
6. Assumptions and limitations must be stated explicitly.
7. HTML, Markdown, DOCX, and graphical interfaces must use the same calculation engine.

## Milestone v0.1.0

Version `v0.1.0` is limited to sample-size estimation for a single population proportion.

### Included

- Base sample-size calculation
- Optional non-response adjustment
- Input validation
- Structured calculation result
- Formula and substitution text
- Markdown and HTML output
- Unit tests
- Worked documentation

### Excluded

- Finite population correction
- Design-effect adjustment
- Cluster sampling
- Two-proportion calculations
- Mean-based calculations
- DOCX output
- Graphical user interface

## Definition of done

The milestone is complete when:

- all linked issues are closed;
- implementation changes are merged through reviewed pull requests;
- automated tests pass;
- worked calculations have been independently verified;
- Markdown and HTML outputs render correctly;
- assumptions and limitations are documented; and
- GitHub prerelease `v0.1.0` is published.