# Plot data are computed by the same engines as the report and result cards.
# Infeasible points are NA so lines never bridge invalid planning regions.
trial_arguments <- function(x) {
  shared <- list(alpha=x$alpha,objective=x$objective,direction=x$direction,margin=x$margin,
    lower=x$lower,upper=x$upper,group1=x$group1,group2=x$group2,
    power=if(is.null(x$power)) .8 else x$power,ratio=x$ratio,nonresponse=0)
  if(inherits(x,"two_means_result")) c(list(mean1=x$mean1,mean2=x$mean2,sd1=x$sd1,sd2=x$sd2),shared)
  else c(list(p1=x$p1,p2=x$p2),shared)
}
curve_data <- function(values, fun) data.frame(x=values,y=vapply(values,function(v) tryCatch(fun(v),error=function(e) NA_real_),numeric(1)))
planning_plots <- function(x) {
  curve <- function(df,xlabel,ylabel,title,mark=NULL,target=NULL) list(kind="curve",data=df,xlab=xlabel,ylab=ylabel,title=title,mark=mark,target=target)
  if(inherits(x,"two_group_result")) {
    fun <- if(inherits(x,"two_means_result")) two_means else two_proportions
    args <- trial_arguments(x); ratio <- x$ratio
    nseq <- unique(round(seq(max(2,x$n1*.15),max(10,x$n1*2),length.out=65)))
    pow <- curve_data(nseq,function(n) { a<-args;a$n1<-n;a$n2<-max(2,ceiling(n*ratio)); do.call(fun,a)$achieved_power*100 })
    pow$x <- pow$x+pmax(2,ceiling(pow$x*ratio))
    names <- paste(x$group2,"minus",x$group1)
    expected <- x$difference; span <- max(abs(expected)*2,if(x$objective=="equivalence") x$upper-x$lower else x$margin*3,x$se_alt*5)
    effects <- seq(expected-span,expected+span,length.out=65)
    if(inherits(x,"two_proportions_result")) effects <- sort(unique(c(effects[effects> -x$p1 & effects<1-x$p1],expected)))
    sz <- curve_data(effects,function(d) {a<-args; if(inherits(x,"two_means_result")) a$mean2<-x$mean1+d else a$p2<-x$p1+d;do.call(fun,a)$n_complete})
    factor <- if(inherits(x,"two_proportions_result")) 100 else 1
    sz$x <- sz$x*factor
    unit <- if(factor==100) "percentage points" else "outcome units"
    boundary <- if(x$objective=="equality") 0 else if(x$objective=="noninferiority") -x$direction_sign*x$margin else if(x$objective=="superiority") x$direction_sign*x$margin else c(x$lower,x$upper)
    # Rejection limits on the signed estimator scale, distinct from clinical margins.
    reject <- if(x$objective=="equality") c(-1,1)*x$z_alpha*x$se_null else if(x$objective=="equivalence") c(x$lower+x$z_alpha*x$se_alt,x$upper-x$z_alpha*x$se_alt) else boundary+x$direction_sign*x$z_alpha*x$se_alt
    hypothesis <- list(kind="hypothesis",title=paste(objective_name(x$objective),"planning illustration"),
      xlab=paste(names,paste0("(",unit,")")),difference=expected*factor,
      boundary=boundary*factor,reject=reject*factor,objective=x$objective,sign=x$direction_sign,
      se=x$se_alt*factor,range=range(c(expected-span,expected+span,boundary,reject))*factor)
    result <- list(hypothesis=hypothesis,power=curve(pow,"Total complete participants","Approximate power (%)","Power at complete sample sizes",mark=c(x$n_complete,x$achieved_power*100),target=if(is.null(x$power)) NULL else x$power*100),
      effect=curve(sz,paste(names,paste0("(",unit,")")),"Total complete participants","Sample size by expected effect",mark=c(expected*factor,x$n_complete)))
    if(x$objective %in% c("noninferiority","equivalence")) {
      base <- if(x$objective=="equivalence") max(abs(c(x$lower,x$upper))) else x$margin
      vals <- seq(max(base*.2,.0001),base*2,length.out=65)
      df <- curve_data(vals,function(m) {a<-args; if(x$objective=="equivalence") {a$lower<-x$lower*m/base;a$upper<-x$upper*m/base} else a$margin<-m;do.call(fun,a)$n_complete})
      df$x <- df$x*factor
      result$margin <- curve(df,paste(if(x$objective=="equivalence") "Largest absolute margin (both limits scaled)" else "Non-inferiority margin",paste0("(",unit,")")),"Total complete participants","Sample size by clinical margin",mark=c(base*factor,x$n_complete))
    }
    return(result)
  }
  if(!inherits(x,"extended_result") || x$key=="effects") return(list())
  fun <- switch(x$key,auc=auc_sample,diagnostic=diagnostic_sample,correlation=correlation_sample)
  args <- x$args
  sizing_args <- args; sizing_args$cases<-sizing_args$controls<-sizing_args$n<-sizing_args$n1<-sizing_args$n2<-NULL
  sizing_args$nonresponse<-0
  result <- list()
  if(x$key=="diagnostic") {
    if(args$recruitment=="population") {
      vals <- sort(unique(c(seq(.02,.98,length.out=49),args$prevalence)))
      df <- curve_data(vals,function(v) {a<-sizing_args;a$prevalence<-v;do.call(fun,a)$n_complete})
      result$prevalence <- curve(df,"Disease prevalence","Total complete participants","Recruitment by disease prevalence",mark=c(args$prevalence,do.call(fun,sizing_args)$n_complete))
    }
    if(!startsWith(args$objective,"test_")) {
      active <- if(args$objective=="specificity") args$precision_spec else args$precision_sens
      vals <- seq(active*.3,min(.3,active*2),length.out=45)
      df <- curve_data(vals,function(v) {a<-sizing_args;a$precision_sens<-v;if(args$objective=="joint") a$precision_spec<-args$precision_spec*v/active else a$precision_spec<-v;do.call(fun,a)$n_complete})
      result$precision <- curve(df,"Interval half-width (joint widths scaled)","Total complete participants","Sample size by desired precision")
    }
  }
  if(x$key=="auc" && args$objective=="paired") {
    vals <- sort(unique(c(seq(-.8,.95,length.out=45),args$auc_correlation)))
    df <- curve_data(vals,function(v) {a<-sizing_args;a$auc_correlation<-v;do.call(fun,a)$n_complete})
    result$covariance <- curve(df,"Correlation between estimated AUCs","Total complete participants","Paired AUC covariance sensitivity")
  }
  if(x$key=="auc" && args$objective=="estimate") {
    vals <- seq(args$precision*.3,min(.25,args$precision*2),length.out=45)
    result$precision <- curve(curve_data(vals,function(v) {a<-sizing_args;a$precision<-v;do.call(fun,a)$n_complete}),"AUC interval half-width","Total complete participants","Sample size by AUC precision")
  }
  if(x$key=="correlation") {
    vals <- sort(unique(c(seq(-.85,.85,length.out=55),args$r)))
    result$effect <- curve(curve_data(vals,function(v) {a<-sizing_args;a$r<-v;do.call(fun,a)$n_complete}),"Anticipated Pearson correlation","Total complete participants","Sample size by anticipated correlation")
  }
  if(!is.null(x$achieved_power)) {
    relevant <- if(x$key=="auc") x$strata$complete[1] else if(x$key=="diagnostic") max(x$strata$complete) else x$counts$complete[1]
    vals <- unique(pmax(if(x$key=="correlation") 4 else 2,round(seq(relevant*.2,relevant*2,length.out=55))))
    powers <- curve_data(vals,function(n) {
      a<-args
      if(x$key=="auc") {a$cases<-n;a$controls<-max(2,ceiling(n*args$ratio))}
      if(x$key=="diagnostic") a$n<-n
      if(x$key=="correlation") {a$n1<-n;if(args$objective=="independent") a$n2<-max(4,ceiling(n*if(x$mode=="power") args$n2/args$n1 else args$ratio))}
      do.call(fun,a)$achieved_power*100
    })
    result$power <- curve(powers,if(x$key=="auc") "Complete diseased participants in first cohort" else if(x$key=="diagnostic") "Complete participants in relevant disease stratum" else "Complete participants in first population", "Approximate power (%)","Power by complete sample size",mark=c(relevant,x$achieved_power*100),target=if(is.null(x$power)) NULL else x$power*100)
  }
  result
}

draw_planning_plot <- function(p) {
  old <- par(no.readonly=TRUE);on.exit(par(old),add=TRUE)
  par(mar=c(5.3,5,3.5,1.5),mgp=c(3,1,0),family="sans",cex=.9)
  accent <- "#205c4c"; muted <- "#7c4d38"
  if(p$kind=="hypothesis") {
    range <- p$range; xx<-seq(range[1],range[2],length.out=400)
    yy<-dnorm(xx,p$difference,p$se)
    plot(xx,yy,type="n",xlab=p$xlab,ylab="Planning density",main=p$title)
    valid <- if(p$objective=="equality") xx<p$reject[1]|xx>p$reject[2] else if(p$objective=="equivalence") xx>p$reject[1]&xx<p$reject[2] else if(p$sign==1) xx>p$reject else xx<p$reject
    runs<-rle(valid);ends<-cumsum(runs$lengths);starts<-c(1,head(ends,-1)+1)
    for(i in which(runs$values)) {s<-starts[i]:ends[i];polygon(c(xx[s],rev(xx[s])),c(yy[s],rep(0,length(s))),col="#dbe9e1",border=NA)}
    lines(xx,yy,col=accent,lwd=2)
    abline(v=p$boundary,col="gray40",lty=3);abline(v=p$reject,col=muted,lty=2);abline(v=p$difference,col=accent,lwd=2)
    legend("topright",c("Expected effect","Null or clinical boundary","Rejection limit","Rejection region"),col=c(accent,"gray40",muted,"#8eb29b"),lty=c(1,3,2,1),lwd=c(2,1,1,6),bty="n",cex=.8)
    mtext("Planning illustration; clinical and statistical rejection boundaries differ.",side=1,line=4.1,cex=.72)
  } else {
    df<-p$data; finite<-is.finite(df$y)
    if(!any(finite)) {plot.new();text(.5,.5,"No feasible planning values in this range.");return(invisible(NULL))}
    ylim<-range(c(df$y[finite],p$mark[2],p$target),na.rm=TRUE);if(diff(ylim)==0) ylim<-ylim+c(-.5,.5)
    use_log<-min(df$y[finite])>0 && max(df$y[finite])/min(df$y[finite])>50 && !grepl("power",p$ylab,ignore.case=TRUE)
    plot(df$x,df$y,type="n",xlab=paste(strwrap(p$xlab,70),collapse="\n"),ylab=paste0(p$ylab,if(use_log) " (log scale)" else ""),main=p$title,ylim=ylim,log=if(use_log) "y" else "")
    grid(col="#e6e6e6");lines(df$x,df$y,col=accent,lwd=2)
    if(!is.null(p$target)) abline(h=p$target,col=muted,lty=2)
    if(!is.null(p$mark) && all(is.finite(p$mark))) points(p$mark[1],p$mark[2],pch=19,col=muted,cex=1.1)
    mtext("Complete observations; gaps indicate infeasible inputs. Recruitment adds losses.",side=1,line=4.1,cex=.72)
  }
  invisible(p)
}
plot_png <- function(p,file,width=1600,height=1000,res=180) {
  grDevices::png(file,width=width,height=height,res=res);on.exit(grDevices::dev.off(),add=TRUE);draw_planning_plot(p);invisible(file)
}
plot_appendix <- function(x) {
  plots <- planning_plots(x);if(!length(plots)) return(character())
  if(!requireNamespace("base64enc",quietly=TRUE)) stop("Install base64enc for portable plot reports.",call.=FALSE)
  out <- c("", "## Planning plots", "", "These plots illustrate planning assumptions, not observed study results.", "")
  for(name in names(plots)) {
    f<-tempfile(fileext=".png");plot_png(plots[[name]],f,width=1200,height=760,res=150)
    encoded<-base64enc::base64encode(f);unlink(f)
    out<-c(out,paste0("![",plots[[name]]$title,"](data:image/png;base64,",encoded,"){width=95%}"),"")
  }
  out
}

