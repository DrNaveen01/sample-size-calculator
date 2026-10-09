advanced_inputs <- function() {
  num <- function(id,label,value,min=NULL,max=NULL,step=.01) shiny::numericInput(id,label,value,min=if(is.null(min)) NA else min,max=if(is.null(max)) NA else max,step=step)
  sel <- function(id,label,choices) shiny::selectInput(id,label,choices,selectize=FALSE)
  panel <- shiny::conditionalPanel
  shiny::tagList(
    panel("['auc','diagnostic','correlation','effects'].includes(input.calculator)",
      panel("input.calculator !== 'effects'",
        sel("advanced_mode","Solve for",c("Sample size"="sample_size","Power at fixed complete counts"="power"))),
      panel("input.calculator === 'effects' || (input.calculator === 'auc' && input.auc_objective === 'estimate') || (input.calculator === 'diagnostic' && !input.diag_objective.startsWith('test_')) || (input.calculator === 'correlation' && input.corr_objective === 'estimate')",num("advanced_confidence","Two-sided confidence level (%)",95,.001,99.999,1)),
      panel("!(input.calculator === 'effects' || (input.calculator === 'auc' && input.auc_objective === 'estimate') || (input.calculator === 'diagnostic' && !input.diag_objective.startsWith('test_')) || (input.calculator === 'correlation' && input.corr_objective === 'estimate'))",
        num("advanced_alpha","Significance level alpha (%)",5,.001,49.999,.5),
        sel("advanced_alternative","Test alternative",c("Two-sided difference"="two.sided","Greater than null/comparator"="greater","Less than null/comparator"="less")),
        panel("input.advanced_mode === 'sample_size'",num("advanced_power","Target power (%)",80,50.001,99.999,1)),
        shiny::tags$p(class="field-help","Estimation uses confidence; hypothesis tests use alpha and power. Alpha is two-sided for a difference test and one-sided for greater/less."))),
    panel("input.calculator === 'auc'",
      sel("auc_objective","AUC objective",c("Estimate one AUC"="estimate","Test one AUC"="test","Compare independent AUCs"="independent","Compare paired AUCs"="paired")),
      num("auc_value","Anticipated AUC",.8,.001,.999),
      panel("input.auc_objective === 'estimate'",num("auc_precision","AUC interval half-width",.05,.001,.999)),
      panel("input.auc_objective === 'test'",num("auc_null","Null AUC",.5,.001,.999)),
      panel("['independent','paired'].includes(input.auc_objective)",num("auc_second","Comparator AUC",.75,.001,.999),
        shiny::textInput("auc_name1","First test name","Test 1"),shiny::textInput("auc_name2","Second test name","Test 2")),
      panel("input.auc_objective === 'independent'",num("auc_cohort_ratio","Cohort 2 / cohort 1 size ratio",1,.001,NULL,.1)),
      panel("input.auc_objective === 'paired'",num("auc_rho","Correlation between estimated AUCs",.5,-.999,.999,.05),
        shiny::tags$p(class="field-help","Specify pilot covariance / sqrt(variance 1 × variance 2). Raw test-score correlation is not interchangeable. The report includes covariance sensitivity.")),
      panel("input.advanced_mode === 'sample_size'",sel("auc_recruitment","Recruitment design",c("Separate disease strata"="separate","One population using prevalence"="population")),
        panel("input.auc_recruitment === 'population'",num("auc_prevalence","Disease prevalence (%)",20,.001,99.999,1)),
        panel("input.auc_recruitment === 'separate'",num("auc_ratio","Non-diseased / diseased size ratio",1,.001,NULL,.1))),
      panel("input.advanced_mode === 'power'",num("auc_cases","Complete diseased count in first cohort",100,2,NULL,1),num("auc_controls","Complete non-diseased count in first cohort",100,2,NULL,1)),
      shiny::tags$p(class="method-note","Full-AUC Hanley–McNeil variance approximation. Paired tests share participants; independent tests use separate cohorts.")),
    panel("input.calculator === 'diagnostic'",
      sel("diag_objective","Diagnostic objective",c("Estimate sensitivity"="sensitivity","Estimate specificity"="specificity","Estimate both"="joint","Test sensitivity"="test_sensitivity","Test specificity"="test_specificity")),
      panel("['sensitivity','joint','test_sensitivity'].includes(input.diag_objective)",num("diag_sens","Anticipated sensitivity (%)",85,.001,99.999,1)),
      panel("['specificity','joint','test_specificity'].includes(input.diag_objective)",num("diag_spec","Anticipated specificity (%)",90,.001,99.999,1)),
      panel("!input.diag_objective.startsWith('test_')",
        sel("diag_interval","Planning interval",c("Wilson expected interval"="wilson","Wald normal approximation"="wald")),
        panel("['sensitivity','joint'].includes(input.diag_objective)",num("diag_dsens","Sensitivity interval half-width (pp)",5,.001,99.999,.5)),
        panel("['specificity','joint'].includes(input.diag_objective)",num("diag_dspec","Specificity interval half-width (pp)",5,.001,99.999,.5))),
      panel("input.diag_objective.startsWith('test_')",num("diag_benchmark","Null accuracy benchmark (%)",70,.001,99.999,1)),
      panel("input.advanced_mode === 'sample_size'",sel("diag_recruitment","Recruitment design",c("Population using prevalence"="population","Separate disease strata"="separate")),
        panel("input.diag_recruitment === 'population'",num("diag_prevalence","Disease prevalence (%)",20,.001,99.999,1))),
      panel("input.advanced_mode === 'power'",num("diag_n","Complete count in relevant disease stratum",100,2,NULL,1)),
      shiny::tags$p(class="method-note","Reference-standard disease status defines strata. Population recruitment plans expected quotas; prevalence does not guarantee realised case counts.")),
    panel("input.calculator === 'correlation'",
      sel("corr_objective","Correlation objective",c("Test one Pearson correlation"="test","Estimate Pearson correlation"="estimate","Compare independent correlations"="independent")),
      num("corr_r","Anticipated Pearson correlation",.3,-.999,.999),
      panel("input.corr_objective === 'test'",num("corr_null","Null correlation (0 for no correlation)",0,-.999,.999)),
      panel("input.corr_objective === 'estimate'",sel("corr_scale","Precision scale",c("Correlation scale"="correlation","Fisher z scale"="fisher")),num("corr_precision","Interval half-width",.1,.001,NULL)),
      panel("input.corr_objective === 'independent'",num("corr_second","Second population correlation",.1,-.999,.999),
        shiny::textInput("corr_name1","First population name","Population 1"),shiny::textInput("corr_name2","Second population name","Population 2"),
        panel("input.advanced_mode === 'sample_size'",num("corr_ratio","Population 2 / population 1 size ratio",1,.001,NULL,.1))),
      panel("input.advanced_mode === 'power'",num("corr_n1","Complete participants in first population",100,4,NULL,1),
        panel("input.corr_objective === 'independent'",num("corr_n2","Complete participants in second population",100,4,NULL,1))),
      shiny::tags$p(class="method-note","Uncorrected Fisher z normal approximation. Counts represent participants with complete paired measurements; independent comparisons use separate populations.")),
    panel("input.calculator === 'effects'",
      sel("effects_design","Study design",c("Cohort"="cohort","Trial"="trial","Case-control"="case_control","Cross-sectional"="cross_sectional")),
      shiny::textInput("effects_name1","Exposed or treatment group","Exposed or treatment"),shiny::textInput("effects_name2","Unexposed or control group","Unexposed or control"),
      num("effects_a","a: exposed/treatment, outcome present",20,0,NULL,1),num("effects_b","b: exposed/treatment, outcome absent",80,0,NULL,1),
      num("effects_c","c: unexposed/control, outcome present",40,0,NULL,1),num("effects_d","d: unexposed/control, outcome absent",60,0,NULL,1),
      sel("effects_correction","Zero-cell handling",c("No correction; Fisher OR for zero cells"="none","Explicit 0.5 correction to all cells"="haldane")),
      shiny::tags$p(class="method-note","Outcome present is an adverse event for ARR and NNT/NNH. Case-control risk measures and cross-sectional treatment-benefit measures are suppressed."))
  )
}
advanced_from_inputs <- function(input,type) {
  estimation <- (type=="auc" && input$auc_objective=="estimate") ||
    (type=="diagnostic" && !startsWith(input$diag_objective,"test_")) ||
    (type=="correlation" && input$corr_objective=="estimate")
  fixed <- type!="effects" && !estimation && input$advanced_mode=="power"
  common <- list(confidence=input$advanced_confidence/100,alpha=input$advanced_alpha/100,
    power=if(fixed||estimation) .8 else input$advanced_power/100,alternative=input$advanced_alternative,
    nonresponse=if(fixed) 0 else input$nonresponse_pct/100)
  if(type=="effects") return(effect_table(input$effects_a,input$effects_b,input$effects_c,input$effects_d,
    confidence=input$advanced_confidence/100,design=input$effects_design,correction=input$effects_correction,
    group1=input$effects_name1,group2=input$effects_name2))
  if(type=="auc") return(do.call(auc_sample,c(list(auc=input$auc_value,objective=input$auc_objective,
    null_auc=input$auc_null,auc2=input$auc_second,precision=input$auc_precision,
    ratio=if(fixed||input$auc_recruitment=="population") 1 else input$auc_ratio,
    cohort_ratio=input$auc_cohort_ratio,recruitment=if(fixed) "separate" else input$auc_recruitment,prevalence=input$auc_prevalence/100,
    auc_correlation=if(input$auc_objective=="paired") input$auc_rho else NULL,
    cases=if(fixed) input$auc_cases else NULL,controls=if(fixed) input$auc_controls else NULL,
    group1=if(input$auc_objective %in% c("paired","independent")) input$auc_name1 else "Test 1",
    group2=if(input$auc_objective %in% c("paired","independent")) input$auc_name2 else "Test 2"),common)))
  if(type=="diagnostic") return(do.call(diagnostic_sample,c(list(objective=input$diag_objective,
    sensitivity=input$diag_sens/100,specificity=input$diag_spec/100,precision_sens=input$diag_dsens/100,
    precision_spec=input$diag_dspec/100,interval=input$diag_interval,benchmark=input$diag_benchmark/100,
    recruitment=if(fixed) "separate" else input$diag_recruitment,prevalence=input$diag_prevalence/100,n=if(fixed) input$diag_n else NULL),common)))
  do.call(correlation_sample,c(list(r=input$corr_r,objective=input$corr_objective,r0=input$corr_null,r2=input$corr_second,
    precision=input$corr_precision,precision_scale=input$corr_scale,ratio=if(fixed) 1 else input$corr_ratio,
    n1=if(fixed) input$corr_n1 else NULL,n2=if(fixed && input$corr_objective=="independent") input$corr_n2 else NULL,
    group1=if(input$corr_objective=="independent") input$corr_name1 else "Population 1",group2=if(input$corr_objective=="independent") input$corr_name2 else "Population 2"),common))
}
advanced_summary <- function(x) {
  detail<-function(label,value) shiny::tags$div(shiny::tags$span(label),shiny::tags$strong(value))
  shiny::tags$section(class="result-hero",
    shiny::tags$div(class="hero-label",if(x$mode=="effect") "OBSERVED TABLE TOTAL" else if(x$mode=="power") "APPROXIMATE POWER" else "FINAL RECRUITMENT TARGET"),
    shiny::tags$p(class="objective-label",x$title),
    shiny::tags$div(class="hero-number",if(x$mode=="power") display_power(x$achieved_power) else display_count(x$n_final)),
    shiny::tags$div(class="group-targets",lapply(seq_len(nrow(x$counts)),function(i) shiny::tags$div(shiny::tags$span(x$counts$label[i]),
      shiny::tags$strong(display_count(x$counts$recruitment[i])),shiny::tags$small(paste("Complete:",display_count(x$counts$complete[i])))))),
    shiny::tags$div(class="result-details",detail("Complete total",display_count(x$n_complete)),
      detail("Method",x$method),if(!is.null(x$achieved_power)) detail("Approximate power",display_power(x$achieved_power)) else detail("Confidence",display_percent(x$confidence))),
    if(!is.null(x$strata)) shiny::tags$p(class="power-check",paste(paste(x$strata$label,x$strata$complete,sep=": "),collapse=" · ")),
    if(!is.null(x$limiting)) shiny::tags$p(class="power-check",paste("Limiting endpoint:",x$limiting)),
    if(!is.null(x$achieved_precision)) shiny::tags$p(class="power-check",paste("Anticipated half-width:",display_number(x$achieved_precision))),
    if(length(x$warnings)) shiny::tags$div(class="calculation-warning",paste(x$warnings,collapse=" ")))
}
