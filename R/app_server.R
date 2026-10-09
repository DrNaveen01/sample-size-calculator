calculator_server <- function(input, output, session) {
  result_state <- shiny::reactive({
    tryCatch({
      type <- if (is.null(input$calculator)) "single_proportion" else input$calculator
      if(type %in% c("auc","diagnostic","correlation","effects")) return(list(result=advanced_from_inputs(input,type),error=NULL))
      comparison <- type %in% c("two_proportions", "two_means")
      sizing <- !comparison || is.null(input$comparison_mode) || input$comparison_mode == "sample_size"
      if (sizing) check_number(input$nonresponse_pct, "Expected non-response (%)", 0, 100, FALSE, TRUE)
      nonresponse <- if (sizing) input$nonresponse_pct / 100 else 0
      if (type %in% c("single_proportion", "single_mean")) {
        if (input$confidence_mode == "confidence") {
          check_number(input$confidence_pct, "Confidence level (%)", 0, 100, TRUE, TRUE)
        }
        confidence <- input$confidence_pct / 100
        z <- if (input$confidence_mode == "z") input$z else NULL
      } else if (comparison) {
        check_number(input$alpha_pct, "Significance level (%)", 0, 100, TRUE, TRUE)
        if (sizing) check_number(input$power_pct, "Target power (%)", 50, 100, TRUE, TRUE)
        objective <- if (is.null(input$objective)) "equality" else input$objective
        proportions <- type == "two_proportions"
        margin <- if (objective == "noninferiority") {
          if (proportions) input$ni_pp / 100 else input$ni_mean
        } else if (objective == "superiority") {
          if (proportions) input$superiority_pp / 100 else input$superiority_mean
        } else 0
        comparison_args <- list(alpha = input$alpha_pct / 100, objective = objective,
          direction = if (is.null(input$direction)) "higher" else input$direction, margin = margin,
          lower = if (objective != "equivalence") NULL else if (proportions) input$lower_pp / 100 else input$lower_mean,
          upper = if (objective != "equivalence") NULL else if (proportions) input$upper_pp / 100 else input$upper_mean,
          group1 = if (is.null(input$group1_name)) "Group 1" else input$group1_name,
          group2 = if (is.null(input$group2_name)) "Group 2" else input$group2_name,
          power = if (sizing) input$power_pct / 100 else 0.8,
          ratio = if (sizing) input$allocation_ratio else 1, nonresponse = nonresponse,
          n1 = if (sizing) NULL else input$fixed_n1, n2 = if (sizing) NULL else input$fixed_n2)
      }
      x <- switch(type,
        single_proportion = {
          check_number(input$p_pct, "Expected proportion (%)", 0, 100, TRUE, TRUE)
          relative <- input$precision_type == "relative"
          check_number(if (relative) input$relative_pct else input$absolute_pct,
            if (relative) "Relative margin of error (%)" else "Margin of error (percentage points)", 0, 100, TRUE)
          single_proportion(input$p_pct / 100,
            if (relative) input$relative_pct / 100 else input$absolute_pct / 100,
            confidence, nonresponse, input$precision_type, z)
        },
        single_mean = single_mean(input$mean_sd, input$mean_precision, confidence, nonresponse, z),
        yamane = {
          check_number(input$yamane_precision_pct, "Precision (percentage points)", 0, 100, TRUE, TRUE)
          yamane(input$population_size, input$yamane_precision_pct / 100, nonresponse)
        },
        two_proportions = {
          check_number(input$p1_pct, "Group 1 proportion (%)", 0, 100, TRUE, TRUE)
          if (input$effect_type == "proportions") check_number(input$p2_pct, "Group 2 proportion (%)", 0, 100, TRUE, TRUE)
          do.call(two_proportions, c(list(p1 = input$p1_pct / 100,
            p2 = if (input$effect_type == "proportions") input$p2_pct / 100 else NULL,
            effect_type = input$effect_type,
            effect = if (input$effect_type == "or") input$odds_ratio else if (input$effect_type == "rr") input$risk_ratio else NULL), comparison_args))
        },
        two_means = do.call(two_means, c(list(mean1 = input$mean1, mean2 = input$mean2,
          sd1 = input$sd1, sd2 = input$sd2,
          sd_method = if (is.null(input$sd_method)) "separate" else input$sd_method,
          pooled_sd = input$pooled_sd, ref_n1 = input$ref_n1, ref_n2 = input$ref_n2), comparison_args)),
        stop("Choose a supported calculator.", call. = FALSE))
      list(result = x, error = NULL)
    }, error = function(e) list(result = NULL, error = conditionMessage(e)))
  })
  current_result <- shiny::reactive({
    value <- result_state()
    shiny::req(is.null(value$error))
    value$result
  })
  markdown_report <- shiny::reactive(calculation_markdown(current_result()))
  report_filename <- function(extension) {
    x <- current_result()
    paste0(calculation_key(x), if (inherits(x, "two_group_result") && x$objective != "equality") paste0("_", x$objective) else "",
      if (identical(x$mode, "power")) "_power" else "", "_", x$n_final, ".", extension)
  }

  output$comparison_help <- shiny::renderUI({
    objective <- if (is.null(input$objective)) "equality" else input$objective
    text <- switch(objective,
      equality = "Alpha is two-sided. This tests for a difference; a non-significant result does not prove equality or equivalence.",
      superiority = "Alpha is one-sided. Enter 2.5% for a one-sided alpha of 0.025. The selected direction determines improvement.",
      noninferiority = "Alpha is one-sided. Enter 2.5% for a one-sided alpha of 0.025. The margin must be justified clinically.",
      equivalence = "Alpha applies to each one-sided test. Both tests must reject. Alpha 5% per test corresponds to a 90% two-sided confidence interval.")
    shiny::tags$p(class = "field-help", text)
  })

  # Display labels follow the editable names; mathematical indices remain 1 and 2.
  shiny::observe({
    first <- if (is.null(input$group1_name)) "Group 1" else input$group1_name
    second <- if (is.null(input$group2_name)) "Group 2" else input$group2_name
    labels <- list(p1_pct = paste(first, "expected proportion (%)"), p2_pct = paste(second, "expected proportion (%)"),
      mean1 = paste(first, "expected mean"), mean2 = paste(second, "expected mean"),
      sd1 = paste(first, "standard deviation"), sd2 = paste(second, "standard deviation"),
      fixed_n1 = paste(first, "complete sample size"), fixed_n2 = paste(second, "complete sample size"),
      ref_n1 = paste("Reference study", first, "sample size"), ref_n2 = paste("Reference study", second, "sample size"),
      allocation_ratio = paste0("Group size ratio (", second, " / ", first, ")"),
      odds_ratio = paste0("Odds ratio (", second, " / ", first, ")"), risk_ratio = paste0("Risk ratio (", second, " / ", first, ")"))
    for (name in names(labels)) shiny::updateNumericInput(session, name, label = labels[[name]])
    shiny::updateSelectInput(session, "direction", label = paste("Direction of benefit for", second))
  })

  shiny::observe({
    if(!is.null(input$auc_name1)) shiny::updateNumericInput(session,"auc_value",label=paste(input$auc_name1,"anticipated AUC"))
    if(!is.null(input$auc_name2)) shiny::updateNumericInput(session,"auc_second",label=paste(input$auc_name2,"anticipated AUC"))
    if(!is.null(input$corr_name1)) shiny::updateNumericInput(session,"corr_r",label=paste(input$corr_name1,"anticipated Pearson correlation"))
    if(!is.null(input$corr_name2)) shiny::updateNumericInput(session,"corr_second",label=paste(input$corr_name2,"anticipated Pearson correlation"))
  })
  output$result_summary <- shiny::renderUI({
    value <- result_state()
    if (!is.null(value$error)) return(shiny::tags$section(class = "error-panel", role = "alert",
      shiny::tags$h2("Check the inputs"), shiny::tags$p(value$error)))
    x <- value$result
    if(inherits(x,"extended_result")) return(advanced_summary(x))
    comparison <- inherits(x, "two_group_result")
    power_mode <- comparison && x$mode == "power"
    detail <- function(label, value) shiny::tags$div(shiny::tags$span(label), shiny::tags$strong(value))
    details <- if (comparison) {
      shiny::tagList(detail(if (power_mode) "Complete observations" else "Complete total", display_count(x$n_complete)),
        detail(if (power_mode) "Group size ratio" else "Target power", if (power_mode) display_number(x$ratio, 6L) else display_power(x$power)),
        detail("Significance level", display_percent(x$alpha)))
    } else shiny::tagList(detail("Complete observations", display_count(x$n_complete)),
      detail(if (inherits(x, "yamane_result")) "Assumed confidence" else "Confidence level", display_percent(x$confidence)),
      detail("Absolute margin", if (inherits(x, "single_proportion_result") || inherits(x, "yamane_result")) paste0(display_number(100 * x$d, 6L), " pp") else display_number(x$d, 6L)))
    shiny::tags$section(class = "result-hero",
      shiny::tags$div(class = "hero-label", if (power_mode) "APPROXIMATE POWER" else "FINAL RECRUITMENT TARGET"),
      if (comparison) shiny::tags$p(class = "objective-label", objective_name(x$objective)),
      shiny::tags$div(class = "hero-number", if (power_mode) display_power(x$achieved_power) else display_count(x$n_final),
        if (!power_mode) shiny::tags$span("participants")),
      if (comparison) shiny::tags$div(class = "group-targets",
        shiny::tags$div(shiny::tags$span(x$group1), shiny::tags$strong(display_count(if (power_mode) x$n1 else x$final1)),
          shiny::tags$small(if (power_mode) "complete observations" else paste0("Complete target: ", display_count(x$n1)))),
        shiny::tags$div(shiny::tags$span(x$group2), shiny::tags$strong(display_count(if (power_mode) x$n2 else x$final2)),
          shiny::tags$small(if (power_mode) "complete observations" else paste0("Complete target: ", display_count(x$n2))))),
      shiny::tags$div(class = "result-details", details),
      if (comparison && !power_mode) shiny::tags$p(class = "power-check", paste0("Approximate power at rounded complete sizes: ", display_power(x$achieved_power),
        " · Planned ", x$group2, " / ", x$group1, " ratio: ", display_number(x$ratio, 6L))),
      if (inherits(x, "two_means_result") && x$sd_method != "separate") shiny::tags$p(class = "power-check",
        paste0(if (x$sd_method == "reference") "Computed" else "Reported", " pooled SD: ", display_number(x$pooled_sd))),
      if (length(x$warnings)) shiny::tags$div(class = "calculation-warning", role = "status", paste(x$warnings, collapse = " ")))
  })

  output$export_actions <- shiny::renderUI({
    if (!is.null(result_state()$error)) return(NULL)
    shiny::tags$div(class = "export-block",
      shiny::tags$div(class = "export-actions",
        shiny::tags$button(type = "button", class = "copy-button", `data-copy-markdown` = "true", "Copy as Markdown"),
        if (export_available("pdf")) shiny::downloadButton("download_pdf", "Download PDF", class = "export-button"),
        if (export_available("docx")) shiny::downloadButton("download_docx", "Download Word", class = "export-button"),
        shiny::downloadButton("download_markdown", "Save .md", class = "export-button secondary-button")),
      if (!export_available("pdf")) shiny::tags$p(class = "export-help", "PDF export needs Pandoc and LaTeX. See the setup instructions."),
      if (!export_available("docx")) shiny::tags$p(class = "export-help", "Word export needs Pandoc. See the setup instructions."),
      shiny::tags$p(id = "export_copy_status", class = "copy-status", role = "status", `aria-live` = "polite"))
  })
  output$report_preview <- shiny::renderUI({
    if (!is.null(result_state()$error)) return(shiny::tags$p(class = "empty-report", "Enter valid inputs to see the calculation report."))
    shiny::tags$article(class = "calculation-report", shiny::HTML(report_html_fragment(current_result())))
  })
  output$markdown_source <- shiny::renderUI({
    value <- if (is.null(result_state()$error)) markdown_report() else ""
    shiny::tags$textarea(id = "markdown_text", class = "markdown-source", readonly = "readonly",
      spellcheck = "false", `aria-label` = "Markdown calculation report", value)
  })
  shiny::outputOptions(output, "markdown_source", suspendWhenHidden = FALSE)

  output$download_markdown <- shiny::downloadHandler(
    filename = function() report_filename("md"), contentType = "text/markdown; charset=utf-8",
    content = function(file) calculation_export(current_result(), file, "markdown"))
  output$download_docx <- shiny::downloadHandler(
    filename = function() report_filename("docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) shiny::withProgress(message = "Preparing Word report", value = 0.5,
      calculation_export(current_result(), file, "docx")))
  output$download_pdf <- shiny::downloadHandler(
    filename = function() report_filename("pdf"), contentType = "application/pdf",
    content = function(file) shiny::withProgress(message = "Preparing PDF report", value = 0.5,
      calculation_export(current_result(), file, "pdf")))

  plot_state <- shiny::reactive({
    if(!is.null(result_state()$error)) return(list())
    planning_plots(current_result())
  })
  output$plot_controls <- shiny::renderUI({
    p <- plot_state()
    if(!length(p)) return(shiny::tags$p("No planning plots apply to this calculator."))
    shiny::selectInput("plot_choice","Planning illustration",setNames(names(p),vapply(p,function(x)x$title,character(1))),selectize=FALSE)
  })
  chosen_plot <- shiny::reactive({
    p<-plot_state(); shiny::req(length(p)>0)
    key<-input$plot_choice
    if(is.null(key)||!key %in% names(p)) key<-names(p)[1]
    p[[key]]
  })
  output$planning_plot <- shiny::renderPlot({
    if(!length(plot_state())) {plot.new();return(invisible(NULL))}
    draw_planning_plot(chosen_plot())
  },res=120)
  output$plot_downloads <- shiny::renderUI({
    if(!length(plot_state())) return(NULL)
    shiny::tagList(shiny::downloadButton("download_plot","Download plot PNG"),shiny::downloadButton("download_plot_data","Download plot data CSV"))
  })
  output$download_plot <- shiny::downloadHandler(filename=function() paste0(calculation_key(current_result()),"_planning.png"),content=function(file) plot_png(chosen_plot(),file))
  output$download_plot_data <- shiny::downloadHandler(filename=function() "planning-data.csv",content=function(file) {
    p<-chosen_plot()
    data<-if(p$kind=="curve") p$data else data.frame(expected=p$difference,null_boundary=p$boundary,rejection_limit=p$reject)
    write.csv(data,file,row.names=FALSE)
  })
  shiny::observe({
    type<-input$calculator
    estimation<-(identical(type,"auc")&&identical(input$auc_objective,"estimate")) || (identical(type,"diagnostic")&&!is.null(input$diag_objective)&&!startsWith(input$diag_objective,"test_")) || (identical(type,"correlation")&&identical(input$corr_objective,"estimate"))
    if(estimation) shiny::updateSelectInput(session,"advanced_mode",choices=c("Sample size"="sample_size"),selected="sample_size")
    else shiny::updateSelectInput(session,"advanced_mode",choices=c("Sample size"="sample_size","Power at fixed complete counts"="power"))
  })

  # Reset values for the selected calculator while keeping the selection.
  shiny::observeEvent(input$reset, {
    defaults <- list(p_pct = 50, absolute_pct = 5, relative_pct = 10, confidence_pct = 95,
      z = 1.96, nonresponse_pct = 10, mean_sd = 10, mean_precision = 2, p1_pct = 20, p2_pct = 30,
      odds_ratio = 1.714285714285714, risk_ratio = 1.5, mean1 = 100, mean2 = 105,
      sd1 = 15, sd2 = 15, alpha_pct = 5, power_pct = 80, allocation_ratio = 1,
      fixed_n1 = 100, fixed_n2 = 100, ni_pp = 5, ni_mean = 5, superiority_pp = 0, superiority_mean = 0,
      lower_pp = -5, upper_pp = 5, lower_mean = -5, upper_mean = 5, pooled_sd = 15,
      ref_n1 = 50, ref_n2 = 100, population_size = 1000, yamane_precision_pct = 5)
    advanced_defaults <- list(advanced_confidence=95,advanced_alpha=5,advanced_power=80,
      auc_value=.8,auc_precision=.05,auc_null=.5,auc_second=.75,auc_cohort_ratio=1,auc_rho=.5,auc_prevalence=20,auc_ratio=1,auc_cases=100,auc_controls=100,
      diag_sens=85,diag_spec=90,diag_dsens=5,diag_dspec=5,diag_benchmark=70,diag_prevalence=20,diag_n=100,
      corr_r=.3,corr_null=0,corr_second=.1,corr_precision=.1,corr_ratio=1,corr_n1=100,corr_n2=100,effects_a=20,effects_b=80,effects_c=40,effects_d=60)
    defaults <- c(defaults,advanced_defaults)
    advanced_selections <- list(advanced_mode="sample_size",advanced_alternative="two.sided",auc_objective="estimate",auc_recruitment="separate",diag_objective="sensitivity",diag_interval="wilson",diag_recruitment="population",corr_objective="test",corr_scale="correlation",effects_design="cohort",effects_correction="none")
    for(name in names(advanced_selections)) shiny::updateSelectInput(session,name,selected=advanced_selections[[name]])
    for(pair in list(c("auc_name1","Test 1"),c("auc_name2","Test 2"),c("corr_name1","Population 1"),c("corr_name2","Population 2"),c("effects_name1","Exposed or treatment"),c("effects_name2","Unexposed or control"))) shiny::updateTextInput(session,pair[1],value=pair[2])
    for (name in names(defaults)) shiny::updateNumericInput(session, name, value = defaults[[name]])
    shiny::updateRadioButtons(session, "precision_type", selected = "absolute")
    shiny::updateRadioButtons(session, "confidence_mode", selected = "confidence")
    shiny::updateRadioButtons(session, "comparison_mode", selected = "sample_size")
    shiny::updateSelectInput(session, "effect_type", selected = "proportions")
    shiny::updateSelectInput(session, "objective", selected = "equality")
    shiny::updateSelectInput(session, "direction", selected = "higher")
    shiny::updateSelectInput(session, "sd_method", selected = "separate")
    shiny::updateTextInput(session, "group1_name", value = "Group 1")
    shiny::updateTextInput(session, "group2_name", value = "Group 2")
  })
}
