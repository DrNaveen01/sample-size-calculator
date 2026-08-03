# Sample Size Workflow

An R Shiny project for transparent, reproducible, and university-ready sample size calculations.

Most online calculators return only a final sample size. This project will also show the conventional formula, the user's assumptions substituted into that formula, the arithmetic result, and a concise interpretation suitable for research documentation.

## First milestone

The first stable module will calculate the sample size for estimating a single population proportion. It will provide:

- validated assumptions and a calculated sample size;
- the conventional mathematical formula;
- the user's numerical values substituted into the formula;
- a clear written result statement;
- export-ready HTML and Markdown output; and
- a DOCX output suitable for submission or inclusion in a protocol.

## Development principle

The single-proportion workflow will be implemented, tested, and stabilized before additional sample size methods are added. The calculation and explanation will share one underlying result object so that the on-screen answer and every exported format remain consistent.

## Status

Project initialization. Calculator implementation has not yet been added.

See [docs/PROJECT_SCOPE.md](docs/PROJECT_SCOPE.md) for the initial product definition and acceptance criteria.