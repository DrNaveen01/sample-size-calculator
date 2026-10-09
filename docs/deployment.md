# Running and deployment

Open the R project, run `source("scripts/setup.R")`, then `shiny::runApp(".")`. Word export requires Pandoc; PDF requires Pandoc and pdflatex. RStudio normally supplies Pandoc. Existing TeX installations are reused; `INSTALL_TINYTEX=true` enables optional TinyTeX installation through setup.R.

## Pinned environment

Install renv, then run `renv::restore(prompt = FALSE)` to restore the checked environment. `.Rprofile` activation is opt-in through `CALCULATOR_USE_RENV=true`, so opening the project alone does not initiate package downloads. R 4.3.3 and the pinned package versions are recorded in renv.lock.

## Deployment failure and repair

The prior lockfile recorded packages that were not installed in the deployment runner. With rsconnect 1.11.2, that state was reproduced locally as `subscript out of bounds` in `parseRenvDependencies`; package-record lookup found an absent dependency. The corrected lockfile was generated from the tested installed environment. Dependency parsing and `rsconnect::writeManifest()` now pass locally. JSON array-valued dependency fields are accepted by renv; they were not established as the cause of the failure.

The deployment workflow restores renv before parsing application dependencies. It includes templates, styles and modules in its explicit bundle, runs the shared checking workflow first, and is triggered only by `workflow_dispatch`. Pushes and pull requests run checks without publishing.

For a read-only bundle check:

```r
install.packages("rsconnect")
source("scripts/deployment_preflight.R")
```

Preflight generates a local manifest and never calls deployApp. The manifest is not a second installer or alternate project entry point.

Actual publication requires the three GitHub secrets SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET and a separately authorised manual workflow dispatch. `scripts/deploy.R` performs that publication only when explicitly run. No credentials are stored in this repository. No hosted deployment was performed during this update.

References: [rsconnect dependency resolution](https://rstudio.github.io/rsconnect/reference/appDependencies.html), [renv lockfiles](https://pkgs.rstudio.com/renv/articles/lockfile.html), [Posit reproducible deployments](https://docs.posit.co/connect/how-to/use-renv-for-reproducible-r-deployments/).
