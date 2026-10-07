calculator_server <- function(input, output, session) {
  result_state <- shiny::reactive({
    tryCatch({
      # Validate UI units before converting percentages to proportions.
      check_number(input$p_pct, "Expected proportion (%)", 0, 100, TRUE, TRUE)
      check_number(input$nonresponse_pct, "Expected non-response (%)", 0, 100, FALSE, TRUE)
      if (input$precision_type == "relative") {
        check_number(input$relative_pct, "Relative margin of error (%)", 0, 100, TRUE)
      } else {
        check_number(input$absolute_pct, "Margin of error (percentage points)", 0, 100, TRUE)
      }
      if (input$confidence_mode == "confidence") {
        check_number(input$confidence_pct, "Confidence level (%)", 0, 100, TRUE, TRUE)
      }
      list(result = single_proportion(
        p = input$p_pct / 100,
        precision = if (input$precision_type == "relative") input$relative_pct / 100 else input$absolute_pct / 100,
        precision_type = input$precision_type,
        confidence = input$confidence_pct / 100,
        nonresponse = input$nonresponse_pct / 100,
        z = if (input$confidence_mode == "z") input$z else NULL
      ), error = NULL)
    }, error = function(e) list(result = NULL, error = conditionMessage(e)))
  })
  current_result <- shiny::reactive({
    value <- result_state()
    shiny::req(is.null(value$error))
    value$result
  })
  markdown_report <- shiny::reactive(single_proportion_markdown(current_result()))

  output$result_summary <- shiny::renderUI({
    value <- result_state()
    if (!is.null(value$error)) {
      return(shiny::tags$section(class = "error-panel", role = "alert",
        shiny::tags$h2("Check the inputs"), shiny::tags$p(value$error)))
    }
    x <- value$result
    shiny::tags$section(class = "result-hero",
      shiny::tags$div(class = "hero-label", "FINAL RECRUITMENT TARGET"),
      shiny::tags$div(class = "hero-number", display_count(x$n_final), shiny::tags$span("participants")),
      shiny::tags$div(class = "result-details",
        shiny::tags$div(shiny::tags$span("Complete observations"), shiny::tags$strong(display_count(x$n_complete))),
        shiny::tags$div(shiny::tags$span("Confidence level"), shiny::tags$strong(display_percent(x$confidence))),
        shiny::tags$div(shiny::tags$span("Absolute margin"), shiny::tags$strong(paste0(display_number(100 * x$d, 6L), " pp")))
      ),
      if (length(x$warnings)) shiny::tags$div(class = "calculation-warning", role = "status", paste(x$warnings, collapse = " "))
    )
  })

  output$export_actions <- shiny::renderUI({
    if (!is.null(result_state()$error)) return(NULL)
    shiny::tags$div(class = "export-block",
      shiny::tags$div(class = "export-actions",
        shiny::tags$button(type = "button", class = "copy-button", `data-copy-markdown` = "true", "Copy as Markdown"),
        if (export_available("pdf")) shiny::downloadButton("download_pdf", "Download PDF", class = "export-button"),
        if (export_available("docx")) shiny::downloadButton("download_docx", "Download Word", class = "export-button"),
        shiny::downloadButton("download_markdown", "Save .md", class = "export-button secondary-button")
      ),
      if (!export_available("pdf")) shiny::tags$p(class = "export-help", "PDF export is unavailable until Pandoc and LaTeX are installed. See the setup instructions."),
      if (!export_available("docx")) shiny::tags$p(class = "export-help", "Word export is unavailable until Pandoc is installed. See the setup instructions."),
      shiny::tags$p(id = "export_copy_status", class = "copy-status", role = "status", `aria-live` = "polite")
    )
  })

  output$report_preview <- shiny::renderUI({
    if (!is.null(result_state()$error)) {
      return(shiny::tags$p(class = "empty-report", "Enter valid inputs to see the calculation report."))
    }
    shiny::tags$article(class = "calculation-report", shiny::HTML(report_html_fragment(current_result())))
  })
  output$markdown_source <- shiny::renderUI({
    value <- if (is.null(result_state()$error)) markdown_report() else ""
    shiny::tags$textarea(id = "markdown_text", class = "markdown-source", readonly = "readonly",
                         spellcheck = "false", `aria-label` = "Markdown calculation report", value)
  })
  # The source is kept up to date even when the Markdown tab is hidden.
  shiny::outputOptions(output, "markdown_source", suspendWhenHidden = FALSE)

  output$download_markdown <- shiny::downloadHandler(
    filename = function() paste0("single_proportion_", current_result()$n_final, ".md"),
    contentType = "text/markdown; charset=utf-8",
    content = function(file) single_proportion_export(current_result(), file, "markdown")
  )
  output$download_docx <- shiny::downloadHandler(
    filename = function() paste0("single_proportion_", current_result()$n_final, ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      shiny::withProgress(message = "Preparing Word report", value = 0.5,
        single_proportion_export(current_result(), file, "docx"))
    }
  )
  output$download_pdf <- shiny::downloadHandler(
    filename = function() paste0("single_proportion_", current_result()$n_final, ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      shiny::withProgress(message = "Preparing PDF report", value = 0.5,
        single_proportion_export(current_result(), file, "pdf"))
    }
  )
  shiny::observeEvent(input$reset, {
    shiny::updateNumericInput(session, "p_pct", value = 50)
    shiny::updateRadioButtons(session, "precision_type", selected = "absolute")
    shiny::updateNumericInput(session, "absolute_pct", value = 5)
    shiny::updateNumericInput(session, "relative_pct", value = 10)
    shiny::updateRadioButtons(session, "confidence_mode", selected = "confidence")
    shiny::updateNumericInput(session, "confidence_pct", value = 95)
    shiny::updateNumericInput(session, "z", value = 1.96)
    shiny::updateNumericInput(session, "nonresponse_pct", value = 10)
  })
}
