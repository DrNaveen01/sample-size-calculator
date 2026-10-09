# Structured, shared report source for the new engines.
extended_markdown <- function(x) {
  reference_link <- function(value) {
    match <- regexpr("https?://[^[:space:]]+", value)
    if(match[1] < 0) return(value)
    url <- regmatches(value, match)
    sub("https?://[^[:space:]]+", paste0("[Methodological source](",url,")"), value)
  }
  d<-display_number;f<-formula_lines
  heading <- if(x$mode=="effect") "Observed table total" else if(x$mode=="power") "Approximate power" else "Final recruitment target"
  headline <- if(x$mode=="power") display_power(x$achieved_power) else display_count(x$n_final)
  lines<-c(paste0("# ",x$title),"",paste0("**",heading,": ",headline,".**"),"",paste0("Method: ",x$method,"."),"",
    "## Generic formulas","",unlist(lapply(x$formulas,f)),
    if(x$mode!="effect") c(f(paste0("n_{i,\\mathrm{adj}}=\\frac{n_{i,\\mathrm{complete}}}{1-",if(x$key=="correlation") "q" else "r","}")),
      "Complete and recruitment targets are shown separately. Recruitment inflation is applied to whole-number complete quotas for these modules. No loss adjustment is applied in fixed-size power mode. $\\Phi$ is the standard-normal CDF; the critical value is two-sided unless the declared alternative is one-sided. $t$ is +1 for greater and -1 for less."),
    "", "## Legends and input values","",legend_header())
  for(k in names(x$inputs)) {
    symbol <- if(grepl("_",k,fixed=TRUE)) sub("_(.*)$","_{\\1}",k) else k
    if(k=="P_star") symbol <- "P_{\\star}"
    symbol <- switch(symbol, alpha="\\alpha",rho="\\rho",pi="\\pi",P_star="P_{\\star}",symbol)
    lines<-c(lines,legend_row(symbol,x$legends[[k]],d(x$inputs[[k]])))
  }
  derived <- switch(x$key,
    auc=c(m="Complete diseased count",n="Complete non-diseased count",Q_1="AUC variance component A divided by 2 minus A",Q_2="AUC variance component 2 A squared divided by 1 plus A",V="Planning AUC variance",V_D="Variance of the AUC difference",D="AUC 1 minus null or comparator AUC",s_0="Null standard error",s_1="Alternative standard error"),
    diagnostic=c(p="Anticipated sensitivity or specificity",n="Relevant complete disease-stratum count",z="Two-sided confidence quantile",h="Anticipated interval half-width",n_D="Required diseased count",n_ND="Required non-diseased count",N="Complete population cohort target",D="Anticipated accuracy minus null benchmark",s_0="Null standard error",s_1="Alternative standard error"),
    correlation=c(z="Fisher transformed correlation",s="Standard error on the Fisher scale",D="Anticipated Fisher difference (second term is comparator for independent samples)",n="Complete participant count",n_1="Complete first population count",n_2="Complete second population count"),
    effects=c(R_1="Outcome proportion in first group",R_0="Outcome proportion in second group",OR="First group versus second group odds ratio",RR="First group versus second group risk ratio",RD="First group minus second group risk difference",ARR="Second group minus first group risk",AF_e="Attributable fraction among exposed",SE="Standard error"))
  lines<-c(lines,"","Derived symbols: ","",paste0("- $",names(derived),"$: ",derived,"."),
    if(!is.null(x$achieved_power)) c("- $P$: normal rejection probability. $z_\\alpha$ is $\\Phi^{-1}(1-\\alpha/2)$ for a two-sided test or $\\Phi^{-1}(1-\\alpha)$ for a directional test.",
      paste0("Null hypothesis: ",if(x$args$alternative=="two.sided") "$D=0$; alternative $D\\ne0$." else if(x$args$alternative=="greater") "$D\\le0$; alternative $D>0$." else "$D\\ge0$; alternative $D<0$.")))
  if(!is.null(x$args$alternative) && !is.null(x$achieved_power)) {
    lines<-c(lines,"",paste0("Declared alternative: **",x$args$alternative,"**. Alpha is ",if(x$args$alternative=="two.sided") "two-sided." else "one-sided."))
  }
  if(x$key=="auc" && x$objective %in% c("paired","independent")) lines<-c(lines,"",paste0("AUC difference orientation: **",escape_report_text(x$args$group1)," minus ",escape_report_text(x$args$group2),"**."))
  lines<-c(lines,"","## Numerical substitution","",unlist(lapply(x$steps,f)),"","## Complete counts and recruitment","",
    "| Group or stratum | Complete observations | Recruitment target |","|:---|---:|---:|")
  for(i in seq_len(nrow(x$counts))) lines<-c(lines,paste0("| ",escape_report_text(x$counts$label[i])," | ",display_count(x$counts$complete[i])," | ",display_count(x$counts$recruitment[i])," |"))
  if(!is.null(x$strata)) {
    lines<-c(lines,"","Required disease-stratum quotas:","","| Stratum | Complete observations |","|:---|---:|")
    for(i in seq_len(nrow(x$strata))) lines<-c(lines,paste0("| ",escape_report_text(x$strata$label[i])," | ",display_count(x$strata$complete[i])," |"))
  }
  if(x$mode!="effect") {
    lines<-c(lines,"",f(paste0("n_{\\mathrm{complete}}=",x$n_complete,",\\qquad n_{\\mathrm{final}}=",x$n_final)))
    if(x$mode!="power") for(i in seq_len(nrow(x$counts))) lines<-c(lines,f(paste0("n_{\\mathrm{adj},",i,"}=\\frac{",x$counts$complete[i],"}{1-",d(x$nonresponse),"}\\approx",d(x$counts$complete[i]/(1-x$nonresponse)),",\\quad n_{\\mathrm{final},",i,"}=",x$counts$recruitment[i])))
  }
  if(!is.null(x$measures)) {
    number <- function(v) if(is.na(v)||is.nan(v)) "Unavailable" else if(is.infinite(v)) if(v>0) "Infinity" else "-Infinity" else d(v)
    lines<-c(lines,"","## Effect measures","",paste0("Confidence level: ",display_percent(x$confidence),"."),"",
      "| Measure | Estimate | Lower limit | Upper limit |","|:---|---:|---:|---:|")
    for(i in seq_len(nrow(x$measures))) {v<-x$measures[i,];lines<-c(lines,paste0("| ",v$measure," | ",number(v$estimate)," | ",number(v$lower)," | ",number(v$upper)," |"))}
    lines<-c(lines,"",x$nnt_text)
  }
  lines<-c(lines,"","## Interpretation", "",
    if(x$mode=="effect") "Effect measures use the explicitly labelled table and study design. Confidence limits and undefined quantities are displayed separately." else if(x$mode=="power") paste0("With the supplied complete counts, approximate power is ",display_power(x$achieved_power)," for the prespecified anticipated effect. This is not evidence derived from an observed study effect.") else paste0("Plan ",display_count(x$n_complete)," complete observations and recruit ",display_count(x$n_final)," participants under the declared loss and allocation assumptions."),
    if(!is.null(x$achieved_power) && x$mode!="power") paste0("Approximate power at complete counts: ",display_power(x$achieved_power),"."),
    if(!is.null(x$achieved_precision)) paste0("Achieved anticipated interval half-width: ",d(x$achieved_precision),"."),
    if(!is.null(x$anticipated_interval) && is.numeric(x$anticipated_interval)) paste0("Anticipated interval: ",d(x$anticipated_interval[1])," to ",d(x$anticipated_interval[2]),"."),
    if(!is.null(x$limiting)) paste0("Limiting recruitment endpoint: ",x$limiting,"."),
    "","## Assumptions and limitations","",paste0("- ",x$assumptions),if(length(x$warnings)) paste0("- ",x$warnings),
    "","## References","",paste0(seq_along(x$references),". ",vapply(x$references,reference_link,character(1))),"")
  paste(lines,collapse="\n")
}
