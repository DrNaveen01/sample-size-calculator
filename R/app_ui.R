calculator_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$link(rel = "stylesheet", href = "styles.css"),
      shiny::tags$script(src = "clipboard.js")
    ),
    title = "Single proportion sample size",
    shiny::tags$div(class = "app-shell",
      shiny::tags$header(class = "app-header",
        shiny::tags$div(class = "brand", "RESEARCH METHODS", shiny::tags$span(" / SAMPLE SIZE")),
        shiny::tags$div(class = "version", "SINGLE PROPORTION")
      ),
      shiny::tags$section(class = "intro",
        shiny::tags$p(class = "eyebrow", "PRECISION BASED ESTIMATION"),
        shiny::tags$h1("Plan your sample. Show your calculation."),
        shiny::tags$p(class = "intro-text",
          "Estimate a population proportion with a specified margin of error. Export the formulas, input values, and calculation steps together.")
      ),
      shiny::tags$div(class = "calculator-grid",
        shiny::tags$aside(class = "input-panel",
          shiny::tags$div(class = "panel-heading", shiny::tags$h2("Calculation inputs"),
            shiny::actionButton("reset", "Reset", class = "reset-button")),
          shiny::numericInput("p_pct", "Expected proportion (%)", value = 50, min = 0.001,
                              max = 99.999, step = 1, width = "100%"),
          shiny::tags$p(class = "field-help", "Use 50% when a suitable prior estimate is unavailable."),
          shiny::radioButtons("precision_type", "Precision", choices = c("Absolute" = "absolute", "Relative" = "relative"),
                              selected = "absolute", inline = TRUE),
          shiny::conditionalPanel("input.precision_type === 'absolute'",
            shiny::numericInput("absolute_pct", "Margin of error (percentage points)", value = 5,
                                min = 0.001, max = 100, step = 0.5, width = "100%"),
            shiny::tags$p(class = "field-help", "5 percentage points means an absolute margin of 0.05.")
          ),
          shiny::conditionalPanel("input.precision_type === 'relative'",
            shiny::numericInput("relative_pct", "Relative margin of error (% of proportion)", value = 10,
                                min = 0.001, max = 100, step = 1, width = "100%"),
            shiny::tags$p(class = "field-help", "10% of a proportion of 50% gives 5 percentage points.")
          ),
          shiny::tags$div(class = "form-divider"),
          shiny::radioButtons("confidence_mode", "Critical value", choices = c("Confidence level" = "confidence", "Custom Z" = "z"),
                              selected = "confidence", inline = TRUE),
          shiny::conditionalPanel("input.confidence_mode === 'confidence'",
            shiny::numericInput("confidence_pct", "Two-sided confidence level (%)", value = 95,
                                min = 0.001, max = 99.999, step = 1, width = "100%")
          ),
          shiny::conditionalPanel("input.confidence_mode === 'z'",
            shiny::numericInput("z", "Standard-normal critical value Z", value = 1.96, min = 0.001,
                                step = 0.01, width = "100%"),
            shiny::tags$p(class = "field-help", "The supplied Z is used directly; implied confidence is reported.")
          ),
          shiny::numericInput("nonresponse_pct", "Expected non-response (%)", value = 10,
                              min = 0, max = 99.999, step = 1, width = "100%"),
          shiny::tags$p(class = "field-help", "Enter 0 when no non-response adjustment is needed."),
          shiny::tags$div(class = "input-note", "Results update as you change the inputs.")
        ),
        shiny::tags$main(class = "results-panel",
          shiny::uiOutput("result_summary"),
          shiny::uiOutput("export_actions"),
          shiny::tags$section(class = "report-panel",
            shiny::tabsetPanel(id = "report_tab", type = "tabs",
              shiny::tabPanel("Calculation report", value = "report", shiny::uiOutput("report_preview")),
              shiny::tabPanel("Markdown", value = "markdown",
                shiny::tags$div(class = "markdown-toolbar",
                  shiny::tags$p("Includes LaTeX equations for Quarto and R Markdown."),
                  shiny::tags$button(id = "copy_markdown", type = "button", class = "copy-button", "Copy as Markdown")),
                shiny::uiOutput("markdown_source"),
                shiny::tags$p(id = "copy_status", role = "status", `aria-live` = "polite")
              )
            )
          )
        )
      ),
      shiny::tags$footer(class = "app-footer",
        shiny::tags$span("Normal approximation · Independent observations · Large population"),
        shiny::tags$span("Developed by Dr Naveen Suthar")
      )
    )
  )
}
