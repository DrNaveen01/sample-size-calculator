# Running and deploying the Shiny app

## Local use

Open `sample-size-calculator.Rproj` in RStudio. Run the setup script once, then launch the app:

```r
source("scripts/setup.R")
shiny::runApp(".")
```

RStudio supplies Pandoc. PDF export also needs `pdflatex` from TeX Live, MiKTeX, or TinyTeX. If TeX Live is installed but not found, add its binary directory to PATH before opening RStudio. Word export needs Pandoc but no LaTeX installation.

To install TinyTeX when no LaTeX installation exists:

```r
Sys.setenv(INSTALL_TINYTEX = "true")
source("scripts/setup.R")
```

Markdown and Word exports remain available when PDF prerequisites are absent. The PDF button is shown when its dependencies are available.

## Shiny hosting

Deploy the project directory to a server capable of running R and Shiny, such as Posit Connect, Shiny Server, or shinyapps.io. Static hosting, including GitHub Pages, cannot execute this R app. The server must have Pandoc, a working LaTeX installation, and the project templates to support every download.

For shinyapps.io, configure your account using the official instructions, then deploy from the project root:

```r
install.packages("rsconnect")
rsconnect::deployApp(appDir = ".", appName = "single-proportion-sample-size")
```

Keep account credentials outside the repository. The included `.rscignore` excludes tests, local outputs, and development files from deployment. Check the hosting environment's PDF dependencies before relying on PDF downloads.

## Reproduce reports without opening the app

```r
source("examples/worked_example.R")
```

This generates Markdown, HTML, PDF, and Word reports in `output/`. Each export is generated from the same calculation result and Markdown report. Word equations are native OMML; HTML equations are native MathML. The preview does not fetch equation scripts from an external service.

## Run checks

```r
source("scripts/run_tests.R")
```

References: [Shiny deployment](https://shiny.posit.co/r/deploy.html), [shinyapps.io guide](https://docs.posit.co/shinyapps.io/guide/), [TinyTeX](https://yihui.org/tinytex/).
