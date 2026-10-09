# Additional planning engines. Probabilities are fractions, never percentages.
# Each result stores its declared method and the exact inputs used.
probability <- function(x, label) check_number(x, label, 0, 1, TRUE, TRUE)
whole_count <- function(x, label, minimum = 2) {
  check_number(x, label, minimum, 1e8)
  if (x != floor(x)) stop(label, " must be a whole number.", call. = FALSE)
}
planning_controls <- function(confidence, alpha, power, nonresponse, estimation) {
  if (estimation) probability(confidence, "Confidence level") else {
    check_number(alpha, "Significance level", 0, .5, TRUE, TRUE)
    check_number(power, "Target power", .5, 1, TRUE, TRUE)
  }
  check_number(nonresponse, "Non-response fraction", 0, 1, FALSE, TRUE)
}
# First passing whole-number n: bounded bisection avoids slow linear searches.
minimum_count <- function(predicate, minimum = 2, maximum = 1e8) {
  lo <- minimum - 1; hi <- minimum
  while (!predicate(hi)) {
    lo <- hi; hi <- min(maximum, hi * 2)
    if (hi == maximum && !predicate(hi)) stop("Required sample size exceeds the supported maximum of 100 million.", call. = FALSE)
  }
  while (hi - lo > 1) {
    mid <- floor((lo + hi) / 2)
    if (predicate(mid)) hi <- mid else lo <- mid
  }
  hi
}
normal_test_power <- function(effect, se0, se1, alpha, alternative = "two.sided") {
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  z <- qnorm(1 - alpha / if (alternative == "two.sided") 2 else 1)
  if (alternative == "two.sided") return(pnorm((effect-z*se0)/se1) + pnorm((-effect-z*se0)/se1))
  pnorm((if (alternative == "greater") effect else -effect)/se1-z*se0/se1)
}
wilson_interval <- function(p, n, confidence = .95) {
  z <- qnorm((1+confidence)/2)
  center <- (p+z^2/(2*n))/(1+z^2/n)
  half <- z*sqrt(p*(1-p)/n+z^2/(4*n^2))/(1+z^2/n)
  c(max(0, center-half), min(1, center+half))
}
extended_result <- function(key, title, method, inputs, formulas, steps, legends,
                            counts, assumptions, references, args, extra = list()) {
  structure(c(list(key=key, title=title, method=method, inputs=inputs,
    formulas=formulas, steps=steps, legends=legends, counts=counts,
    n_complete=sum(counts$complete), n_final=sum(counts$recruitment),
    assumptions=assumptions, references=references, args=args, warnings=character()),extra),
    class=c(paste0(key,"_result"),"extended_result"))
}
# Hanley-McNeil full-AUC variance. m=diseased, n=non-diseased.
auc_variance <- function(auc, m, n) {
  q1 <- auc/(2-auc); q2 <- 2*auc^2/(1+auc)
  (auc*(1-auc)+(m-1)*(q1-auc^2)+(n-1)*(q2-auc^2))/(m*n)
}
auc_sample <- function(auc = .8, objective = c("estimate","test","independent","paired"),
                       null_auc = .5, auc2 = .75, precision = .05,
                       confidence = .95, alpha = .05, power = .8,
                       alternative = "two.sided", ratio = 1, cohort_ratio = 1,
                       recruitment = c("separate","population"), prevalence = .2,
                       auc_correlation = NULL, nonresponse = 0, cases = NULL, controls = NULL,
                       group1 = "Test 1", group2 = "Test 2") {
  objective <- match.arg(objective); recruitment <- match.arg(recruitment)
  alternative <- match.arg(alternative,c("two.sided","greater","less"))
  estimation <- objective == "estimate"; fixed <- !is.null(cases)||!is.null(controls)
  probability(auc,"Anticipated AUC")
  if (!estimation) {
    if (objective == "test") probability(null_auc,"Null AUC") else probability(auc2,"Comparator AUC")
  }
  if (fixed && estimation) stop("AUC estimation solves for sample size; fixed-size power requires a test objective.",call.=FALSE)
  planning_controls(confidence,alpha,if(fixed) .8 else power,if(fixed) 0 else nonresponse,estimation)
  if (estimation) check_number(precision,"AUC interval half-width",0,1,TRUE,TRUE)
  if (recruitment == "population") { probability(prevalence,"Disease prevalence"); ratio <- (1-prevalence)/prevalence }
  check_number(ratio,"Non-diseased / diseased ratio",0,Inf,TRUE)
  if(objective == "independent") check_number(cohort_ratio,"Cohort 2 / cohort 1 ratio",0,Inf,TRUE)
  if(objective == "paired") check_number(auc_correlation,"Correlation between estimated AUCs",-1,1,TRUE,TRUE)
  names <- comparison_group_names(group1,group2)
  if(fixed) { whole_count(cases,"Complete diseased count"); whole_count(controls,"Complete non-diseased count"); ratio <- controls/cases; nonresponse <- 0 }
  effect <- if(estimation) NA_real_ else auc-if(objective == "test") null_auc else auc2
  if(!estimation && !fixed && (effect==0 || (alternative=="greater" && effect<=0) || (alternative=="less" && effect>=0))) stop("Expected AUC difference must satisfy the selected alternative for sample-size planning.",call.=FALSE)
  counts_at <- function(m, n=ceiling(m*ratio)) {
    if(objective=="independent") c(m,n,max(2,ceiling(m*cohort_ratio)),max(2,ceiling(n*cohort_ratio))) else c(m,n)
  }
  se_at <- function(cc, null=FALSE) {
    a1 <- if(null && objective=="test") null_auc else if(null && objective %in% c("independent","paired")) (auc+auc2)/2 else auc
    v1 <- auc_variance(a1,cc[1],cc[2])
    if(objective %in% c("estimate","test")) return(sqrt(v1))
    a2 <- if(null) a1 else auc2
    v2 <- auc_variance(a2, if(objective=="independent") cc[3] else cc[1], if(objective=="independent") cc[4] else cc[2])
    sqrt(v1+v2-if(objective=="paired") 2*auc_correlation*sqrt(v1*v2) else 0)
  }
  power_at <- function(cc) normal_test_power(effect,se_at(cc,TRUE),se_at(cc),alpha,alternative)
  z <- qnorm((1+confidence)/2)
  m <- if(fixed) cases else minimum_count(function(m) {
    cc <- counts_at(m,max(2,ceiling(m*ratio)))
    if(estimation) z*se_at(cc)<=precision else power_at(cc)>=power
  })
  cc <- counts_at(m,if(fixed) controls else max(2,ceiling(m*ratio)))
  label <- if(objective=="independent") c(paste(names$group1,"diseased"),paste(names$group1,"non-diseased"),paste(names$group2,"diseased"),paste(names$group2,"non-diseased")) else c("Diseased","Non-diseased")
  # Population recruitment plans expected strata; it does not guarantee realised counts.
  counts <- if(recruitment=="population" && !fixed) {
    total <- max(cc[1]/prevalence,cc[2]/(1-prevalence))
    if(objective=="independent") total <- c(total,max(cc[3]/prevalence,cc[4]/(1-prevalence)))
    data.frame(label=if(length(total)==2) c(names$group1,names$group2) else "Population cohort",complete=ceiling(total),recruitment=ceiling(total/(1-nonresponse)))
  } else data.frame(label=label,complete=cc,recruitment=ceiling(cc/(1-nonresponse)))
  inputs <- c(A=auc,if(objective=="test") c(A_0=null_auc),if(objective %in% c("paired","independent")) c(A_2=auc2),
    k=ratio,if(objective=="independent") c(k_c=cohort_ratio),if(objective=="paired") c(rho=auc_correlation),
    if(estimation) c(C=confidence,d=precision) else c(alpha=alpha,if(!fixed) c(P_star=power)),r=nonresponse,
    if(recruitment=="population") c(pi=prevalence))
  formulas <- c("Q_1=A/(2-A),\\quad Q_2=2A^2/(1+A)",
    "V(A;m,n)=\\frac{A(1-A)+(m-1)(Q_1-A^2)+(n-1)(Q_2-A^2)}{mn}",
    if(objective=="paired") "V_D=V_1+V_2-2\\rho\\sqrt{V_1V_2}" else if(objective=="independent") "V_D=V_1+V_2" else "s=\\sqrt{V(A;m,n)}",
    if(estimation) "Z_{(1+C)/2}\\sqrt{V(A;m,n)}\\le d" else if(alternative=="two.sided") "P=\\Phi((D-z_\\alpha s_0)/s_1)+\\Phi((-D-z_\\alpha s_0)/s_1)" else "P=\\Phi((tD-z_\\alpha s_0)/s_1)")
  fmt <- function(v) display_number(v)
  steps <- c(paste0("Q_1=",fmt(auc),"/(2-",fmt(auc),")=",fmt(auc/(2-auc))),
    paste0("Q_2=2(",fmt(auc),")^2/(1+",fmt(auc),")=",fmt(2*auc^2/(1+auc))),
    paste0("V_1=\\frac{",fmt(auc*(1-auc)),"+(",cc[1],"-1)(",fmt(auc/(2-auc)-auc^2),")+(",cc[2],"-1)(",fmt(2*auc^2/(1+auc)-auc^2),")}{",cc[1],"\\times",cc[2],"}=",fmt(auc_variance(auc,cc[1],cc[2]))),
    paste0("s_1=",fmt(se_at(cc))))
  if(!estimation) steps <- c(steps,paste0("D=",fmt(auc),"-",fmt(if(objective=="test") null_auc else auc2),"=",fmt(effect)),paste0("s_0=",fmt(se_at(cc,TRUE))),paste0("P_{\\mathrm{approx}}=",fmt(power_at(cc))))
  if(estimation) steps <- c(steps,paste0("d_{\\mathrm{achieved}}=",fmt(z),"\\times",fmt(se_at(cc)),"=",fmt(z*se_at(cc))))
  if(objective %in% c("paired","independent")) {
    m2<-if(objective=="independent") cc[3] else cc[1];n2<-if(objective=="independent") cc[4] else cc[2]
    v1<-auc_variance(auc,cc[1],cc[2]);v2<-auc_variance(auc2,m2,n2)
    steps<-c(steps,paste0("V_2=V(",fmt(auc2),";",m2,",",n2,")=",fmt(v2)),
      if(objective=="paired") paste0("\\operatorname{Cov}_{12}=",fmt(auc_correlation),"\\sqrt{",fmt(v1),"\\times",fmt(v2),"}=",fmt(auc_correlation*sqrt(v1*v2))),
      paste0("V_D=",fmt(v1),"+",fmt(v2),if(objective=="paired") paste0("-2\\times",fmt(auc_correlation*sqrt(v1*v2))) else "","=",fmt(se_at(cc)^2)))
  }
  if(!estimation) {
    za<-qnorm(1-alpha/if(alternative=="two.sided") 2 else 1)
    steps<-c(steps,paste0("z_\\alpha=",fmt(za)),
      if(alternative=="two.sided") paste0("P=\\Phi\\left(\\frac{",fmt(effect),"-",fmt(za),"\\times",fmt(se_at(cc,TRUE)),"}{",fmt(se_at(cc)),"}\\right)+\\Phi\\left(\\frac{-",fmt(effect),"-",fmt(za),"\\times",fmt(se_at(cc,TRUE)),"}{",fmt(se_at(cc)),"}\\right)=",fmt(power_at(cc))) else
      paste0("P=\\Phi\\left(\\frac{",fmt(if(alternative=="greater") effect else -effect),"-",fmt(za),"\\times",fmt(se_at(cc,TRUE)),"}{",fmt(se_at(cc)),"}\\right)=",fmt(power_at(cc))))
  }
  assumptions <- c("Full ROC AUC only. Hanley-McNeil variance is a planning approximation, not a DeLong or exact calculation. Independent participants and a suitable reference standard are required.",
    "Diseased m and non-diseased n are independent strata. The minimum whole-number diseased count is found by bounded bisection; other stratum counts are rounded upward.",
    if(objective=="paired") "Both tests are measured in the same participants. rho is the correlation between estimated AUCs, not the correlation between raw test measurements. It is held constant during resizing and must come from pilot covariance or a justified assumption. A sensitivity plot varies rho." else "Independent AUC comparisons use separate participant cohorts; counts are not shared.",
    "For testing, null variance is evaluated at the null AUC (single test) or common mean AUC (comparison). Alternative variance uses the anticipated AUCs. The normal rejection probability is solved numerically.",
    "Population recruitment counts plan expected disease strata using prevalence. They do not guarantee that the realised sample contains the required cases; monitor recruitment or recruit by strata.",
    "For estimation, precision means normal-interval half-width on the AUC scale. Boundary clipping is not used to claim narrower precision.")
  refs <- c("Hanley JA, McNeil BJ. Radiology. 1982;143:29-36. https://doi.org/10.1148/radiology.143.1.7063747",
    "DeLong ER et al. Biometrics. 1988;44:837-845. https://doi.org/10.2307/2531595 (paired covariance context; not the variance engine used here).")
  args <- as.list(environment())[c("auc","objective","null_auc","auc2","precision","confidence","alpha","power","alternative","ratio","cohort_ratio","recruitment","prevalence","auc_correlation","nonresponse","cases","controls","group1","group2")]
  result <- extended_result("auc",paste("ROC AUC",objective,"planning"),"Hanley-McNeil variance with normal planning",inputs,formulas,steps,
    c(A="Anticipated AUC",A_0="Null AUC",A_2="Comparator AUC",k="Non-diseased / diseased ratio",k_c="Cohort 2 / cohort 1 ratio",rho="Correlation of estimated AUCs",C="Two-sided confidence",d="AUC interval half-width",alpha="Significance level",P_star="Target power",r="Non-response fraction",pi="Disease prevalence"),counts,assumptions,refs,args,
    list(objective=objective,mode=if(fixed) "power" else "sample_size",achieved_power=if(estimation) NULL else power_at(cc),
      achieved_precision=if(estimation) z*se_at(cc) else NULL,strata=data.frame(label=label,complete=cc),
      anticipated_interval=if(estimation) auc+c(-1,1)*z*se_at(cc) else NULL,power=if(estimation||fixed) NULL else power,
      ratio=ratio,confidence=confidence,nonresponse=nonresponse,se_alt=se_at(cc)))
  if(estimation && any(result$anticipated_interval < 0 | result$anticipated_interval > 1)) result$warnings <- "The anticipated normal interval extends beyond [0,1]; consider simulation or a transformed AUC interval."
  result
}

diagnostic_sample <- function(objective=c("sensitivity","specificity","joint","test_sensitivity","test_specificity"),
                              sensitivity=.85,specificity=.9,precision_sens=.05,precision_spec=.05,
                              confidence=.95,interval=c("wilson","wald"), benchmark=.7,
                              alpha=.05,power=.8,alternative="greater",recruitment=c("population","separate"),
                              prevalence=.2,nonresponse=0,n=NULL) {
  objective <- match.arg(objective); interval <- match.arg(interval); recruitment <- match.arg(recruitment)
  alternative <- match.arg(alternative,c("two.sided","greater","less"))
  estimation <- !startsWith(objective,"test_"); fixed <- !is.null(n)
  if(fixed && estimation) stop("Fixed-size power requires a sensitivity or specificity test objective.",call.=FALSE)
  planning_controls(confidence,alpha,if(fixed) .8 else power,if(fixed) 0 else nonresponse,estimation)
  active_sens <- objective %in% c("sensitivity","joint","test_sensitivity")
  active_spec <- objective %in% c("specificity","joint","test_specificity")
  if(active_sens) probability(sensitivity,"Anticipated sensitivity")
  if(active_spec) probability(specificity,"Anticipated specificity")
  if(recruitment=="population") probability(prevalence,"Disease prevalence")
  z <- qnorm((1+confidence)/2)
  half_at <- function(p,n) if(interval=="wald") z*sqrt(p*(1-p)/n) else diff(wilson_interval(p,n,confidence))/2
  size <- function(p,d) {
    check_number(d,"Interval half-width",0,1,TRUE,TRUE)
    minimum_count(function(n) half_at(p,n)<=d)
  }
  if(estimation) {
    ns <- if(active_sens) size(sensitivity,precision_sens) else 0
    nt <- if(active_spec) size(specificity,precision_spec) else 0
    achieved <- NULL
  } else {
    probability(benchmark,"Null sensitivity or specificity")
    p <- if(active_sens) sensitivity else specificity
    effect <- p-benchmark
    if(!fixed && (effect==0 || (alternative=="greater" && effect<=0) || (alternative=="less" && effect>=0))) stop("Expected accuracy must satisfy the selected alternative relative to the benchmark.",call.=FALSE)
    p_at <- function(n) normal_test_power(effect,sqrt(benchmark*(1-benchmark)/n),sqrt(p*(1-p)/n),alpha,alternative)
    if(fixed) { whole_count(n,"Complete relevant-stratum count"); nonresponse <- 0 }
    required <- if(fixed) n else minimum_count(function(n) p_at(n)>=power)
    ns <- if(active_sens) required else 0; nt <- if(active_spec) required else 0
    achieved <- p_at(required)
  }
  strata <- data.frame(label=c("Diseased","Non-diseased"),complete=c(ns,nt))
  if(recruitment=="population" && !fixed) {
    requirements <- c(if(ns>0) ns/prevalence else 0,if(nt>0) nt/(1-prevalence) else 0)
    total <- ceiling(max(requirements))
    counts <- data.frame(label="Population cohort",complete=total,recruitment=ceiling(total/(1-nonresponse)))
    limiting <- if(requirements[1]==requirements[2]) "Both endpoints" else if(requirements[1]>requirements[2]) "Sensitivity" else "Specificity"
  } else {
    counts <- data.frame(label=strata$label[strata$complete>0],complete=strata$complete[strata$complete>0],recruitment=ceiling(strata$complete[strata$complete>0]/(1-nonresponse)))
    limiting <- if(ns>0 && nt>0) "Separate quotas" else if(ns>0) "Sensitivity" else "Specificity"
  }
  inputs <- c(if(active_sens) c(Se=sensitivity),if(active_spec) c(Sp=specificity),
    if(estimation) c(C=confidence,if(active_sens) c(d_Se=precision_sens),if(active_spec) c(d_Sp=precision_spec)) else c(p_0=benchmark,alpha=alpha,if(!fixed) c(P_star=power)),
    if(recruitment=="population") c(pi=prevalence),r=nonresponse)
  formulas <- c(if(estimation && interval=="wald") "n=Z_{(1+C)/2}^2p(1-p)/d^2" else if(estimation) "h(n)=\\frac{z\\sqrt{p(1-p)/n+z^2/(4n^2)}}{1+z^2/n}\\le d" else "s_0=\\sqrt{p_0(1-p_0)/n},\\quad s_1=\\sqrt{p_1(1-p_1)/n}",
    if(!estimation) if(alternative=="two.sided") "P=\\Phi((D-z_\\alpha s_0)/s_1)+\\Phi((-D-z_\\alpha s_0)/s_1)" else "P=\\Phi((tD-z_\\alpha s_0)/s_1)",
    if(recruitment=="population") "N=\\max(n_D/\\pi,n_{ND}/(1-\\pi))")
  steps <- c(paste0("n_D=",ns,",\\quad n_{ND}=",nt))
  if(estimation) {
    if(ns>0) steps <- c(steps,paste0("h_{Se}(",ns,")=",display_number(half_at(sensitivity,ns))))
    if(nt>0) steps <- c(steps,paste0("h_{Sp}(",nt,")=",display_number(half_at(specificity,nt))))
  } else steps <- c(steps,paste0("D=",display_number(p),"-",display_number(benchmark),"=",display_number(effect)),
    paste0("s_0=\\sqrt{",display_number(benchmark),"(1-",display_number(benchmark),")/",required,"}=",display_number(sqrt(benchmark*(1-benchmark)/required))),
    paste0("s_1=\\sqrt{",display_number(p),"(1-",display_number(p),")/",required,"}=",display_number(sqrt(p*(1-p)/required))),paste0("P_{\\mathrm{approx}}=",display_number(achieved)))
  if(recruitment=="population" && !fixed) steps <- c(steps,paste0("N=\\max(",ns,"/",display_number(prevalence),",",nt,"/(1-",display_number(prevalence),"))\\approx",display_number(max(requirements))))
  if(estimation) {
    for(pair in list(c(sensitivity,ns),c(specificity,nt))) {
      p<-pair[1];nn<-pair[2]
      if(nn>0) steps<-c(steps,if(interval=="wilson") paste0("h=\\frac{",display_number(z),"\\sqrt{",display_number(p),"(1-",display_number(p),")/",nn,"+",display_number(z),"^2/(4\\times",nn,"^2)}}{1+",display_number(z),"^2/",nn,"}=",display_number(half_at(p,nn))) else
        paste0("h=",display_number(z),"\\sqrt{",display_number(p),"(1-",display_number(p),")/",nn,"}=",display_number(half_at(p,nn))))
    }
  } else {
    za<-qnorm(1-alpha/if(alternative=="two.sided") 2 else 1)
    s0<-sqrt(benchmark*(1-benchmark)/required);s1<-sqrt(p*(1-p)/required)
    steps<-c(steps,paste0("z_\\alpha=",display_number(za)),
      if(alternative=="two.sided") paste0("P=\\Phi(",display_number((effect-za*s0)/s1),")+\\Phi(",display_number((-effect-za*s0)/s1),")=",display_number(achieved)) else
        paste0("P=\\Phi(",display_number((if(alternative=="greater") effect else -effect)/s1-za*s0/s1),")=",display_number(achieved)))
  }
  assumptions <- c("Sensitivity requires reference-standard diseased participants; specificity requires reference-standard non-diseased participants. Choose a valid reference standard and prespecified index-test threshold.",
    "Precision is the interval half-width, not total width. Wilson planning evaluates the expected proportion at each candidate count; it does not guarantee the realised interval width. Wald planning is Buderer's normal approximation.",
    "For tests, a normal score rejection threshold uses the null proportion variance and anticipated alternative variance. This is not an exact binomial test; small expected cell counts may invalidate it.",
    "Joint precision planning uses individual confidence levels for each endpoint; it does not provide simultaneous confidence coverage. The largest population requirement governs recruitment.",
    "Population counts use expected prevalence and do not guarantee realised disease quotas. Separate recruitment meets each quota directly; predictive values cannot be inferred from artificial stratum proportions.",
    "Recruitment inflation is applied to the rounded complete quota so expected analysable counts meet each required stratum quota.")
  args <- as.list(environment())[c("objective","sensitivity","specificity","precision_sens","precision_spec","confidence","interval","benchmark","alpha","power","alternative","recruitment","prevalence","nonresponse","n")]
  result <- extended_result("diagnostic",paste("Diagnostic accuracy",gsub("_"," ",objective),"planning"),
    if(estimation) paste(if(interval=="wilson") "Wilson expected-interval" else "Wald Buderer","precision planning") else "Normal score approximation for one accuracy proportion",
    inputs,formulas,steps,c(Se="Anticipated sensitivity",Sp="Anticipated specificity",C="Two-sided confidence",d_Se="Sensitivity interval half-width",d_Sp="Specificity interval half-width",p_0="Null benchmark",alpha="Significance level",P_star="Target power",pi="Disease prevalence",r="Non-response fraction"),counts,assumptions,
    c("Buderer NM. Acad Emerg Med. 1996;3:895-900. https://doi.org/10.1111/j.1553-2712.1996.tb03538.x",
      "Wilson EB. J Am Stat Assoc. 1927;22:209-212. https://doi.org/10.1080/01621459.1927.10502953"),args,
    list(objective=objective,mode=if(fixed) "power" else "sample_size",achieved_power=achieved,power=if(estimation||fixed) NULL else power,
      strata=strata,limiting=limiting,confidence=confidence,nonresponse=nonresponse,
      anticipated_interval=if(estimation) lapply(which(strata$complete>0),function(i) if(interval=="wilson") wilson_interval(if(i==1) sensitivity else specificity,strata$complete[i],confidence) else (if(i==1) sensitivity else specificity)+c(-1,1)*half_at(if(i==1) sensitivity else specificity,strata$complete[i])) else NULL))
  if(!estimation && min(required*p,required*(1-p),required*benchmark,required*(1-benchmark))<10) result$warnings <- "Fewer than 10 expected events or non-events under a planning hypothesis; normal score power may be unreliable."
  result
}

correlation_sample <- function(r=.3,objective=c("test","estimate","independent"),r0=0,r2=.1,
                               precision=.1,precision_scale=c("correlation","fisher"),confidence=.95,
                               alpha=.05,power=.8,alternative="two.sided",ratio=1,nonresponse=0,
                               n1=NULL,n2=NULL,group1="Population 1",group2="Population 2") {
  objective <- match.arg(objective); precision_scale <- match.arg(precision_scale)
  alternative <- match.arg(alternative,c("two.sided","greater","less"))
  check_number(r,"Anticipated Pearson correlation",-1,1,TRUE,TRUE)
  estimation <- objective=="estimate"; fixed <- !is.null(n1)||!is.null(n2)
  if(estimation && fixed) stop("Correlation estimation solves for sample size; choose a test objective for fixed-size power.",call.=FALSE)
  planning_controls(confidence,alpha,if(fixed) .8 else power,if(fixed) 0 else nonresponse,estimation)
  if(objective=="test") check_number(r0,"Null correlation",-1,1,TRUE,TRUE)
  if(objective=="independent") { check_number(r2,"Second population correlation",-1,1,TRUE,TRUE); check_number(ratio,"Population 2 / population 1 ratio",0,Inf,TRUE) }
  groups <- comparison_group_names(group1,group2)
  zr <- atanh(r); zc <- if(estimation) NA else atanh(if(objective=="test") r0 else r2)
  effect <- zr-zc; z <- qnorm((1+confidence)/2)
  interval_at <- function(n) tanh(zr+c(-1,1)*z/sqrt(n-3))
  half_at <- function(n) if(precision_scale=="fisher") z/sqrt(n-3) else diff(interval_at(n))/2
  sizes_at <- function(n) if(objective=="independent") c(n,max(4,ceiling(n*ratio))) else n
  power_at <- function(ns) {
    se <- sqrt(1/(ns[1]-3)+if(objective=="independent") 1/(ns[2]-3) else 0)
    normal_test_power(effect,se,se,alpha,alternative)
  }
  if(fixed) { whole_count(n1,"Complete population 1 count",4); if(objective=="independent") whole_count(n2,"Complete population 2 count",4); nonresponse <- 0 }
  if(!estimation && !fixed && (effect==0 || (alternative=="greater" && effect<=0) || (alternative=="less" && effect>=0))) stop("Expected Fisher-z difference must satisfy the selected alternative.",call.=FALSE)
  if(estimation) check_number(precision,"Interval half-width",0,if(precision_scale=="correlation") 1 else Inf,TRUE)
  base <- if(fixed) n1 else minimum_count(function(n) if(estimation) half_at(n)<=precision else power_at(sizes_at(n))>=power,minimum=4)
  ns <- if(fixed && objective=="independent") c(n1,n2) else sizes_at(base)
  counts <- data.frame(label=if(length(ns)==2) c(groups$group1,groups$group2) else "Complete participant pairs",complete=ns,recruitment=ceiling(ns/(1-nonresponse)))
  inputs <- c(r=r,if(objective=="test") c(r_0=r0),if(objective=="independent") c(r_2=r2,k=if(fixed) n2/n1 else ratio),
    if(estimation) c(C=confidence,d=precision) else c(alpha=alpha,if(!fixed) c(P_star=power)),q=nonresponse)
  formulas <- c("z(r)=\\operatorname{atanh}(r)=\\frac{1}{2}\\log\\frac{1+r}{1-r}",
    if(objective=="independent") "s=\\sqrt{1/(n_1-3)+1/(n_2-3)}" else "s=1/\\sqrt{n-3}",
    if(estimation) "CI_r=\\tanh(z(r)\\pm Z_{(1+C)/2}/\\sqrt{n-3})" else if(objective=="independent") "D=z(r)-z(r_2),\\quad H_0:D=0" else "D=z(r)-z(r_0),\\quad H_0:D=0",
    if(estimation) if(precision_scale=="fisher") "h_z=Z_{(1+C)/2}/\\sqrt{n-3}\\le d" else "h_r=(CI_{upper}-CI_{lower})/2\\le d" else if(alternative=="two.sided") "P=\\Phi(D/s-z_\\alpha)+\\Phi(-D/s-z_\\alpha)" else "P=\\Phi(tD/s-z_\\alpha)")
  steps <- c(paste0("z(r)=\\operatorname{atanh}(",display_number(r),")=",display_number(zr)),paste0("n_1=",ns[1],if(length(ns)==2) paste0(",\\quad n_2=",ns[2]) else ""))
  if(estimation) steps <- c(steps,paste0("h=",display_number(half_at(ns[1]))),paste0("CI_r=[",display_number(interval_at(ns[1])[1]),",",display_number(interval_at(ns[1])[2]),"]")) else steps <- c(steps,
    paste0("D=",display_number(zr),"-(",display_number(zc),")=",display_number(effect)),
    paste0("s=",display_number(sqrt(1/(ns[1]-3)+if(length(ns)==2) 1/(ns[2]-3) else 0))),paste0("P_{\\mathrm{approx}}=",display_number(power_at(ns))))
  if(!estimation) {
    se<-sqrt(1/(ns[1]-3)+if(length(ns)==2) 1/(ns[2]-3) else 0);za<-qnorm(1-alpha/if(alternative=="two.sided") 2 else 1)
    steps<-c(steps,paste0("z_\\alpha=",display_number(za)),if(alternative=="two.sided") paste0("P=\\Phi(",display_number(effect/se-za),")+\\Phi(",display_number(-effect/se-za),")=",display_number(power_at(ns))) else paste0("P=\\Phi(",display_number((if(alternative=="greater") effect else -effect)/se-za),")=",display_number(power_at(ns))))
  }
  args <- as.list(environment())[c("r","objective","r0","r2","precision","precision_scale","confidence","alpha","power","alternative","ratio","nonresponse","n1","n2","group1","group2")]
  extended_result("correlation",paste("Pearson correlation",objective,"planning"),"Uncorrected Fisher-z normal approximation",inputs,formulas,steps,
    c(r="Anticipated Pearson correlation",r_0="Null Pearson correlation",r_2="Second population correlation",k="Population 2 / population 1 ratio",C="Two-sided confidence",d=paste("Interval half-width on",precision_scale,"scale"),alpha="Significance level",P_star="Target power",q="Non-response fraction"),counts,
    c("Independent participant pairs and approximate bivariate normality are assumed. Each participant provides two measurements; sample size counts participants, not individual measurements.",
      "This uses the uncorrected Fisher-z approximation with variance 1/(n-3). It is not the exact Pearson t-test power or the bias-corrected pwr.r.test method.",
      "For correlation-scale precision, d is half the total anticipated interval width; the interval need not be symmetric about r. For Fisher-scale precision, d is half-width on the transformed scale.",
      "Independent correlation comparisons require separate participant populations. Dependent correlations, Spearman correlation and repeated measures are deferred.",
      "Complete counts are rounded upward; recruitment inflation is applied to these complete participant quotas."),
    c("Fisher RA. Metron. 1921;1:3-32. https://digital.library.adelaide.edu.au/items/002ad8fb-c23c-407b-8a89-f036d8da6030",
      "R stats cor.test documentation, Fisher confidence interval. https://search.r-project.org/R/refmans/stats/html/cor.test.html",
      "R pwr.r.test documentation, bias-corrected alternative method. https://search.r-project.org/CRAN/refmans/pwr/html/pwr.r.test.html"),args,
    list(objective=objective,mode=if(fixed) "power" else "sample_size",achieved_power=if(estimation) NULL else power_at(ns),power=if(estimation||fixed) NULL else power,
      anticipated_interval=if(estimation) interval_at(ns[1]) else NULL,achieved_precision=if(estimation) half_at(ns[1]) else NULL,confidence=confidence,nonresponse=nonresponse))
}
