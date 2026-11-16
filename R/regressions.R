.regression_tables<-function(modelFormula,data,ciWidth) {

  estimates <- try({
    model<-stats::lm(modelFormula, data=data)
    smr<-summary(model)
    pvalue=pf(smr$fstatistic[[1]],smr$fstatistic[[2]],smr$fstatistic[[3]],lower.tail = F)
    anov<-list(rsquared=smr$r.squared,f=smr$fstatistic[[1]],df1=smr$fstatistic[[2]],df2=smr$fstatistic[[3]],p=pvalue)
    res<-as.data.frame(smr$coefficients)
    res$df<-smr$df[2]
    ci<-confint(model,level = ciWidth)
    res<-cbind(res,ci)
    colnames(res)<-c("estimate","se","t","p","df","cilow","cihig")
    res
  })
  if (jmvcore::isError(estimates)) {
    msg<-jmvcore::extractErrorMessage(estimates)
    jmvcore::reject(msg, code='error')
  }

  toscale<-all.vars(as.formula(modelFormula))
  zdata<-as.data.frame(scale(data[,toscale]))
  beta<-stats::lm(modelFormula, data=zdata)
  estimates$beta<-beta$coefficients
  list(estimates[-1,],anov)
}

# Match the factor-dummy labels used in the main model and contrasts table.
.term_label<-function(term) {
  jmvcore::stringifyTerm(gsub(FACTOR_SYMBOL,"",term,fixed=TRUE))
}

# Return layouts, not jamovi tables; SmartArray dispatches each child payload.
regression_layout <- function(dep, terms, estimator, total = FALSE) {
    rows <- lapply(estimator$expandTerms(terms), function(term) {
        label <- .term_label(term)
        if (total) list(source = label, label = label) else list(lhs = dep, rhs = label)
    })
    anova <- if (total) list(dep = dep) else list(lhs = dep)
    list(anova = list(anova), regression = rows)
}

# Preserve the existing B64/factor-aware fitting, returning values for SmartTabs.
regression_values <- function(dep, terms, estimator, data, ciWidth, total = FALSE) {
    formula <- estimator$modelFormula64(dep, terms)
    tables <- .regression_tables(formula, data, ciWidth)
    layout <- regression_layout(dep, terms, estimator, total)
    rows <- lapply(seq_along(layout$regression), function(index) {
        c(layout$regression[[index]], as.list(tables[[1]][index, , drop = FALSE]))
    })
    list(anova = list(c(layout$anova[[1]], tables[[2]])), regression = rows)
}
