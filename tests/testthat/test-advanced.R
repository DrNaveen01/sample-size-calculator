testthat::test_that("new engines match independently integrated reference fixtures", {
  fixture<-read.csv("../fixtures/independent-advanced.csv")
  for(i in seq_len(nrow(fixture))) {
    v<-fixture[i,]
    if(v$kind=="auc") {
      x<-auc_sample(auc=v$a,auc2=v$a2,null_auc=v$a2,objective=v$objective,auc_correlation=if(v$objective=="paired") v$rho else NULL,ratio=v$k)
      testthat::expect_equal(x$strata$complete[1:2],c(v$n1,v$n2),info=paste(v$kind,v$objective))
      testthat::expect_equal(if(v$objective=="estimate") x$achieved_precision else x$achieved_power,v$check,tolerance=1e-9)
    } else if(v$kind=="diagnostic") {
      x<-diagnostic_sample(objective=if(v$objective=="test") "test_sensitivity" else "sensitivity",sensitivity=v$a,precision_sens=v$a2,benchmark=v$a2,
        interval=if(v$objective=="normal") "wald" else "wilson",recruitment="separate")
      testthat::expect_equal(x$strata$complete[1],v$n1)
      if(v$objective=="test") testthat::expect_equal(x$achieved_power,v$check,tolerance=1e-9)
    } else {
      x<-correlation_sample(r=v$a,r0=v$a2,r2=v$a2,objective=v$objective,ratio=v$k)
      testthat::expect_equal(x$counts$complete[1],v$n1)
      if(v$objective=="independent") testthat::expect_equal(x$counts$complete[2],v$n2)
      testthat::expect_equal(if(v$objective=="estimate") x$achieved_precision else x$achieved_power,v$check,tolerance=1e-9)
    }
  }
})
testthat::test_that("effect orientation and intervals match statsmodels", {
  fixture<-read.csv("../fixtures/independent-effects.csv")
  x<-effect_table(20,80,40,60)
  testthat::expect_equal(x$measures$estimate[1:3],fixture$estimate,tolerance=1e-10)
  testthat::expect_equal(x$measures$lower[1:3],fixture$lower,tolerance=1e-10)
  testthat::expect_equal(x$measures$upper[1:3],fixture$upper,tolerance=1e-10)
  testthat::expect_equal(effect_table(40,60,20,80)$measures$estimate[1],1/x$measures$estimate[1])
  testthat::expect_equal(nrow(effect_table(20,80,40,60,design="case_control")$measures),1)
  testthat::expect_equal(nrow(effect_table(20,80,40,60,design="cross_sectional")$measures),3)
  testthat::expect_match(effect_table(20,80,21,79)$nnt_text,"infinity")
  testthat::expect_match(effect_table(20,80,20,80)$nnt_text,"infinite")
  testthat::expect_false(effect_table(0,100,20,80)$correction_applied)
  testthat::expect_true(effect_table(0,100,20,80,correction="haldane")$correction_applied)
  testthat::expect_equal(effect_table(0,100,20,80)$measures$upper[1],fisher.test(matrix(c(0,100,20,80),2,byrow=TRUE))$conf.int[2])
})
testthat::test_that("active validation and impossible hypotheses fail clearly", {
  testthat::expect_error(auc_sample(objective="paired"),"Correlation")
  testthat::expect_error(auc_sample(objective="test",null_auc=.8),"alternative")
  testthat::expect_error(auc_sample(auc=1),"less than")
  testthat::expect_error(diagnostic_sample(prevalence=0),"prevalence")
  testthat::expect_error(correlation_sample(r=1),"less than")
  testthat::expect_error(correlation_sample(r=.2,r0=.3,alternative="greater"),"alternative")
  testthat::expect_error(effect_table(0,0,1,2),"totals")
  testthat::expect_error(effect_table(.5,1,2,3),"whole")
  testthat::expect_error(correlation_sample(n1=3),"at least 4")
  testthat::expect_s3_class(diagnostic_sample(objective="sensitivity",specificity=NA,precision_spec=NA,recruitment="separate",prevalence=NA),"extended_result")
  testthat::expect_s3_class(auc_sample(auc2=NA,null_auc=NA,auc_correlation=NA),"extended_result")
  testthat::expect_s3_class(correlation_sample(objective="estimate",r0=NA,r2=NA),"extended_result")
})
testthat::test_that("complete quotas, recruitment and achieved power agree", {
  for(x in list(auc_sample(objective="paired",auc_correlation=.5,nonresponse=.1),diagnostic_sample(objective="joint",nonresponse=.1),correlation_sample(objective="independent",r=.5,r2=.2,ratio=2,nonresponse=.1))) {
    testthat::expect_equal(x$n_complete,sum(x$counts$complete))
    testthat::expect_equal(x$n_final,sum(x$counts$recruitment))
    testthat::expect_true(all(x$counts$recruitment>=x$counts$complete))
    if(!is.null(x$achieved_power)) testthat::expect_gte(x$achieved_power,.8)
    testthat::expect_true(all(names(x$inputs)%in%names(x$legends)))
  }
  x<-diagnostic_sample(objective="joint",prevalence=.2)
  testthat::expect_equal(x$n_complete,max(ceiling(x$strata$complete[1]/.2),ceiling(x$strata$complete[2]/.8)))
  x<-auc_sample(objective="paired",auc_correlation=.8);y<-auc_sample(objective="paired",auc_correlation=.2)
  testthat::expect_lt(x$n_complete,y$n_complete)
  x<-correlation_sample(r=.3);y<-correlation_sample(r=-.3)
  testthat::expect_equal(x$n_complete,y$n_complete)
})
testthat::test_that("trial plot curves retain infeasible gaps and match results", {
  for(x in list(two_means(100,105,15),two_proportions(.2,.21,objective="equivalence",lower=-.05,upper=.04),two_means(100,100,15,objective="noninferiority",margin=5))) {
    p<-planning_plots(x)
    testthat::expect_equal(p$power$mark,c(x$n_complete,100*x$achieved_power))
    testthat::expect_equal(p$effect$mark[2],x$n_complete)
    testthat::expect_true(length(p)>=3)
  }
  p<-planning_plots(two_means(0,1,1,objective="superiority",margin=.5))
  testthat::expect_true(any(is.na(p$effect$data$y)))
})
