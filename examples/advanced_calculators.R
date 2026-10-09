# Run from the repository root; preserves the established report pipeline.
for(f in c("single_proportion","planning","report","reports_extended","reports_objectives","exports","advanced","effects","plots","reports_advanced")) source(paste0("R/",f,".R"))
dir.create("output/advanced",recursive=TRUE,showWarnings=FALSE)
scenarios <- list(
  auc_estimate=auc_sample(auc=.8,precision=.05),
  auc_test=auc_sample(auc=.8,null_auc=.65,objective="test",ratio=2,nonresponse=.1),
  auc_independent=auc_sample(auc=.8,auc2=.7,objective="independent",ratio=1.5,cohort_ratio=2),
  auc_paired=auc_sample(auc=.8,auc2=.75,objective="paired",auc_correlation=.5,nonresponse=.1),
  auc_power=auc_sample(auc=.8,auc2=.75,objective="paired",auc_correlation=.5,cases=200,controls=400),
  sensitivity=diagnostic_sample(objective="sensitivity"),
  specificity=diagnostic_sample(objective="specificity",recruitment="separate"),
  diagnostic_joint=diagnostic_sample(objective="joint",nonresponse=.1),
  diagnostic_test_sens=diagnostic_sample(objective="test_sensitivity",alternative="greater"),
  diagnostic_test_spec=diagnostic_sample(objective="test_specificity",benchmark=.8,alternative="greater"),
  diagnostic_power=diagnostic_sample(objective="test_sensitivity",n=100,recruitment="separate"),
  correlation_zero=correlation_sample(r=.3),
  correlation_nonzero=correlation_sample(r=.5,r0=.2),
  correlation_estimate=correlation_sample(r=.3,objective="estimate",precision=.1,nonresponse=.1),
  correlation_fisher_precision=correlation_sample(r=.3,objective="estimate",precision=.15,precision_scale="fisher"),
  correlation_independent=correlation_sample(r=.5,r2=.2,objective="independent",ratio=2),
  correlation_power=correlation_sample(r=.5,r2=.2,objective="independent",n1=80,n2=160),
  effects_trial=effect_table(20,80,40,60,design="trial"),
  effects_case_control=effect_table(20,80,40,60,design="case_control"),
  effects_zero=effect_table(0,100,20,80,correction="haldane"),
  effects_null_crossing=effect_table(20,80,21,79)
)
representatives <- c("auc_paired","diagnostic_joint","correlation_estimate","effects_trial")
summary <- data.frame()
for(name in names(scenarios)) {
  x <- scenarios[[name]]
  formats <- c("markdown","html",if(name %in% representatives) c("pdf","docx"))
  for(format in formats) {
    extension <- if(format=="markdown") "md" else format
    calculation_export(x,file.path("output/advanced",paste0(name,".",extension)),format)
  }
  plots<-planning_plots(x)
  for(k in names(plots)) {
    plot_png(plots[[k]],file.path("output/advanced",paste0(name,"_",k,".png")))
    if(plots[[k]]$kind=="curve") write.csv(plots[[k]]$data,file.path("output/advanced",paste0(name,"_",k,".csv")),row.names=FALSE)
  }
  summary<-rbind(summary,data.frame(scenario=name,complete=x$n_complete,recruitment=x$n_final,power=if(is.null(x$achieved_power)) NA else x$achieved_power))
  cat(name,"complete",x$n_complete,"recruitment",x$n_final,"\n")
}
write.csv(summary,"output/advanced/scenario-summary.csv",row.names=FALSE)
