# Updating the GitHub repository from this bundle

The updated source includes all five calculators, the four two-group objectives, power modes, pooled SD options, custom group names, scrolling improvements, and shared exports. It incorporates the README badge and workflow added on main. The GitHub connection rejected branch creation with HTTP 403 ("Resource not accessible by integration"), so no remote branch or pull request was created for this extension.

Copy the project files into your repository checkout, review the changes, and run:

```r
source("scripts/run_tests.R")
source("examples/all_calculators.R")
```

The workflow has been consolidated into `.github/workflows/R-CMD-check.yaml`, which now runs application checks and generates all worked reports. Remove the previous `.github/workflows/checks.yaml` from the checkout when applying this bundle. The source is a Shiny app; the workflow does not invoke R CMD check on a package.

The `output` folder contains example reports and can be regenerated. It is excluded from git. Open sample-size-calculator.Rproj in RStudio and use `shiny::runApp(".")` to launch the interface.
