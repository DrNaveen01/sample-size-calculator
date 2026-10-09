testthat::test_that("Shiny advanced state switches modes and clears invalid inputs", {
  shiny::testServer(calculator_server, {
    session$setInputs(calculator="auc",auc_objective="paired",auc_value=.8,auc_second=.75,auc_rho=.5,
      auc_null=.5,auc_precision=.05,auc_ratio=1,auc_cohort_ratio=1,auc_recruitment="separate",auc_prevalence=20,
      auc_name1="Standard test",auc_name2="New test",auc_cases=200,auc_controls=400,
      advanced_confidence=95,advanced_alpha=5,advanced_power=80,advanced_alternative="two.sided",advanced_mode="sample_size",nonresponse_pct=10)
    testthat::expect_equal(current_result()$n_final,758)
    testthat::expect_match(markdown_report(),"Numerical substitution")
    session$setInputs(auc_rho=1)
    testthat::expect_null(result_state()$result)
    testthat::expect_match(result_state()$error,"Correlation")
    session$setInputs(auc_rho=.5,advanced_mode="power",nonresponse_pct=100,advanced_power=100)
    testthat::expect_equal(current_result()$mode,"power")
    testthat::expect_equal(current_result()$n_complete,600)
    testthat::expect_equal(current_result()$n_final,600)
    session$setInputs(calculator="diagnostic",diag_objective="joint",diag_sens=85,diag_spec=90,diag_dsens=5,diag_dspec=5,
      diag_interval="wilson",diag_recruitment="population",diag_prevalence=20,advanced_mode="sample_size",nonresponse_pct=10)
    testthat::expect_equal(current_result()$n_final,1089)
    session$setInputs(diag_prevalence=0)
    testthat::expect_null(result_state()$result)
    session$setInputs(calculator="correlation",corr_objective="test",corr_r=.3,corr_null=0,corr_second=.1,corr_ratio=1,corr_precision=.1,corr_scale="correlation",advanced_mode="sample_size",advanced_power=80,nonresponse_pct=0)
    testthat::expect_equal(current_result()$n_complete,85)
    session$setInputs(corr_r=1)
    testthat::expect_null(result_state()$result)
    session$setInputs(calculator="effects",effects_a=20,effects_b=80,effects_c=40,effects_d=60,effects_design="case_control",effects_correction="none",effects_name1="Cases exposed",effects_name2="Controls exposed")
    testthat::expect_equal(nrow(current_result()$measures),1)
    testthat::expect_match(markdown_report(),"Odds ratio")
    testthat::expect_false(grepl("\\| Risk ratio \\|",markdown_report()))
  })
})
testthat::test_that("portable Markdown, Word and PDF preserve equations and plots", {
  x<-auc_sample(objective="paired",auc_correlation=.5)
  md<-calculation_markdown(x)
  testthat::expect_match(md,"data:image/png;base64")
  testthat::expect_match(md,"\\\\rho")
  testthat::expect_match(md,"Null hypothesis")
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  if(export_available("docx")) {
    f<-file.path(d,"report.docx");calculation_export(x,f,"docx",reference_doc="../../templates/reference.docx")
    unzip(f,exdir=file.path(d,"word"))
    xml<-paste(readLines(file.path(d,"word/word/document.xml"),warn=FALSE),collapse="")
    testthat::expect_match(xml,"m:oMath")
    testthat::expect_match(xml,"w:drawing")
    testthat::expect_gt(length(list.files(file.path(d,"word/word/media"))),0)
  }
  if(export_available("pdf")) {
    f<-file.path(d,"report.pdf");calculation_export(x,f,"pdf",reference_doc="../../templates/reference.docx")
    testthat::expect_gt(file.info(f)$size,10000)
  }
})
