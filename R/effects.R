# Row 1: exposed/treatment; row 2: unexposed/control.
# Column 1: outcome present; column 2: outcome absent.
effect_table <- function(a,b,c,d,confidence=.95,design=c("cohort","trial","case_control","cross_sectional"),
                         correction=c("none","haldane"),group1="Exposed or treatment",group2="Unexposed or control") {
  design <- match.arg(design); correction <- match.arg(correction)
  groups <- comparison_group_names(group1,group2)
  cells <- c(a,b,c,d)
  for(i in seq_along(cells)) { check_number(cells[i],paste("Cell",letters[i]),0,1e8); if(cells[i]!=floor(cells[i])) stop("Cell counts must be whole numbers.",call.=FALSE) }
  if(a+b==0 || c+d==0) stop("Both group totals must be positive.",call.=FALSE)
  probability(confidence,"Confidence level")
  original <- matrix(cells,2,byrow=TRUE,dimnames=list(c(groups$group1,groups$group2),c("Outcome present","Outcome absent")))
  applied <- correction=="haldane" && any(cells==0)
  if(applied) cells <- cells+.5
  a <- cells[1]; b <- cells[2]; c <- cells[3]; d <- cells[4]
  z <- qnorm((1+confidence)/2); p1 <- a/(a+b); p0 <- c/(c+d)
  or <- a*d/(b*c); rr <- p1/p0; rd <- p1-p0
  ratio_ci <- function(value,se) if(is.finite(value) && value>0 && is.finite(se)) exp(log(value)+c(-1,1)*z*se) else c(NA_real_,NA_real_)
  orci <- ratio_ci(or,sqrt(sum(1/cells)))
  rrci <- ratio_ci(rr,sqrt(1/a-1/(a+b)+1/c-1/(c+d)))
  if(correction=="none" && any(cells==0)) {
    fisher <- stats::fisher.test(original,conf.level=confidence)
    or <- unname(fisher$estimate); orci <- fisher$conf.int[1:2]
  }
  i1 <- wilson_interval(p1,a+b,confidence); i0 <- wilson_interval(p0,c+d,confidence)
  rdci <- c(rd-sqrt((p1-i1[1])^2+(i0[2]-p0)^2),rd+sqrt((i1[2]-p1)^2+(p0-i0[1])^2))
  row <- function(measure,estimate,ci) data.frame(measure=measure,estimate=estimate,lower=ci[1],upper=ci[2],stringsAsFactors=FALSE)
  measures <- row("Odds ratio",or,orci)
  nnt_text <- NULL
  if(design!="case_control") {
    measures <- rbind(measures,row(if(design=="cross_sectional") "Prevalence ratio" else "Risk ratio",rr,rrci),
      row(if(design=="cross_sectional") "Prevalence difference" else "Risk difference",rd,rdci))
    if(design!="cross_sectional") {
      arr <- -rd; arrci <- -rev(rdci)
      af <- 1-1/rr; afci <- 1-1/rrci
      # AF confidence limits preserve increasing transformation of RR.
      measures <- rbind(measures,row("Absolute risk reduction",arr,arrci),row("Attributable fraction among exposed",af,afci))
      if(rd!=0) {
        nnt <- 1/abs(rd)
        if(rdci[1]>0) { nntci <- rev(1/rdci); nnt_text <- sprintf("NNH %.4g; confidence interval %.4g to %.4g",nnt,nntci[1],nntci[2]) }
        else if(rdci[2]<0) { nntci <- c(1/abs(rdci[1]),1/abs(rdci[2])); nnt_text <- sprintf("NNT %.4g; confidence interval %.4g to %.4g",nnt,nntci[1],nntci[2]) }
        else nnt_text <- paste0(if(rd<0) "NNT" else "NNH"," ",display_number(nnt),"; interval crosses no effect: benefit branch ",
          if(rdci[1]<0) display_number(1/abs(rdci[1])) else "undefined"," to infinity; harm branch ",
          if(rdci[2]>0) display_number(1/rdci[2]) else "undefined"," to infinity.")
      } else nnt_text <- "NNT/NNH is infinite at zero risk difference; no finite point estimate."
    }
  }
  warnings <- if(applied) "Explicit Haldane-Anscombe correction: 0.5 added to all four cells because at least one cell is zero. All displayed corrected measures use that table." else if(any(cells==0)) "No correction applied. Odds ratio uses Fisher's conditional estimate and exact interval for zero cells; RR log interval is unavailable when undefined." else character()
  x <- extended_result("effects","Effect measures from a 2 by 2 table","Log-Wald ratio intervals and Newcombe Wilson risk-difference interval",c(a=original[1,1],b=original[1,2],c=original[2,1],d=original[2,2],C=confidence),
    c("OR=ad/(bc)","SE(\\log OR)=\\sqrt{1/a+1/b+1/c+1/d}",
      if(design!="case_control") c("R_1=a/(a+b),\\quad R_0=c/(c+d),\\quad RR=R_1/R_0", "SE(\\log RR)=\\sqrt{1/a-1/(a+b)+1/c-1/(c+d)}","RD=R_1-R_0", if(design!="cross_sectional") "ARR=-RD,\\quad AF_e=1-1/RR,\\quad NNT/NNH=1/|RD|")),
    c(paste0("OR=\\frac{",a,"\\times",d,"}{",b,"\\times",c,"}\\approx",if(is.finite(a*d/(b*c))) display_number(a*d/(b*c)) else "\\mathrm{undefined}"),
      if(design!="case_control") c(paste0("R_1=",a,"/(",a,"+",b,")=",display_number(p1)),paste0("R_0=",c,"/(",c,"+",d,")=",display_number(p0)),paste0("RD=",display_number(p1),"-",display_number(p0),"=",display_number(rd)))),
    c(a="Group 1 outcome present",b="Group 1 outcome absent",c="Group 2 outcome present",d="Group 2 outcome absent",C="Two-sided confidence"),
    data.frame(label=c(groups$group1,groups$group2),complete=rowSums(original),recruitment=rowSums(original)),
    c("Outcome orientation: first row is exposed or treatment, second row is unexposed or control. The first column is outcome present, defined as an adverse event for ARR and NNT/NNH interpretation.",
      "Independent observations and unadjusted comparisons are assumed. Association does not establish causation; AF and NNT interpretations require a defensible causal contrast and a common follow-up period.",
      "Case-control sampling supports odds ratios; risks, RR, RD, ARR, AF and NNT are suppressed. Cross-sectional sampling displays prevalence ratio and difference; treatment benefit measures are suppressed.",
      "Positive-cell OR and RR intervals use log-Wald approximations. RD uses Newcombe's independent Wilson intervals (method 10); ARR reverses RD limits and AF transforms RR limits.",
      "If RD confidence limits cross zero, NNT/NNH confidence sets are disjoint and unbounded; they are never reported as a single finite interval.",warnings),
    c("Newcombe RG. Stat Med. 1998;17:873-890. https://doi.org/10.1002/(SICI)1097-0258(19980430)17:8%3C873::AID-SIM779%3E3.0.CO;2-I",
      "R fisher.test documentation. https://search.r-project.org/R/refmans/stats/html/fisher.test.html",
      "statsmodels Table2x2 documentation (independent validation). https://www.statsmodels.org/stable/generated/statsmodels.stats.contingency_tables.Table2x2.html"),
    list(a=original[1,1],b=original[1,2],c=original[2,1],d=original[2,2],confidence=confidence,design=design,correction=correction,group1=group1,group2=group2),
    list(measures=measures,table=original,corrected_table=matrix(cells,2,byrow=TRUE),nnt_text=nnt_text,mode="effect",confidence=confidence,design=design,correction_applied=applied))
  if(correction=="none" && any(cells==0)) {
    x$method<-"Fisher conditional odds ratio and exact interval; Newcombe Wilson risk difference"
    x$steps[1]<-paste0("OR_{\\mathrm{conditional}}=",if(is.finite(or)) display_number(or) else "\\infty")
  } else {
    seor<-sqrt(sum(1/cells))
    x$steps<-c(x$steps,paste0("SE(\\log OR)=\\sqrt{1/",a,"+1/",b,"+1/",c,"+1/",d,"}=",display_number(seor)),
      paste0("CI_{OR}=\\exp(\\log(",display_number(or),")\\pm",display_number(z),"\\times",display_number(seor),")=[",display_number(orci[1]),",",display_number(orci[2]),"]"))
  }
  if(design!="case_control" && all(is.finite(rrci))) x$steps<-c(x$steps,paste0("RR=",display_number(p1),"/",display_number(p0),"=",display_number(rr)),
    paste0("CI_{RR}=[",display_number(rrci[1]),",",display_number(rrci[2]),"]"),paste0("CI_{RD}=[",display_number(rdci[1]),",",display_number(rdci[2]),"]"))
  x$warnings <- warnings
  x
}
