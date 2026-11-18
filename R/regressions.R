.regression_tables<-function(modelFormula,data,ciWidth) {

  estimates <- try({
    model<-stats::lm(modelFormula, data=data)
    smr<-summary(model)
    pvalue=stats::pf(smr$fstatistic[[1]],smr$fstatistic[[2]],smr$fstatistic[[3]],lower.tail = F)
    anov<-list(rsquared=smr$r.squared,f=smr$fstatistic[[1]],df1=smr$fstatistic[[2]],df2=smr$fstatistic[[3]],p=pvalue)
    res<-as.data.frame(smr$coefficients)
    res$df<-smr$df[2]
    ci<-stats::confint(model,level = ciWidth)
    res<-cbind(res,ci)
    colnames(res)<-c("estimate","se","t","p","df","ci.lower","ci.upper")
    res
  })
  if (jmvcore::isError(estimates)) {
    msg<-jmvcore::extractErrorMessage(estimates)
    jmvcore::reject(msg, code='error')
  }

  toscale<-all.vars(stats::as.formula(modelFormula))
  zdata<-as.data.frame(scale(data[,toscale]))
  beta<-stats::lm(modelFormula, data=zdata)
  estimates$beta<-beta$coefficients
  list(estimates[-1,],anov)
}

# Match the factor-dummy labels used in the main model and contrasts table.
.term_label<-function(term) {
  jmvcore::stringifyTerm(gsub(FACTOR_SYMBOL,"",term,fixed=TRUE))
}

# The comparison a factor contrast makes, from the contrast variable's name,
# plain ("group..f_f_f..1") or encoded: e.g. "2 - 1", the label shown in the
# Contrasts Definition table. NULL when the name is not a factor contrast.
contrast_comparison<-function(name, datamatic) {
  name<-trimws(name)
  if (!grepl(FACTOR_SYMBOL,name,fixed=TRUE))
    return(NULL)
  pieces<-strsplit(name,FACTOR_SYMBOL,fixed=TRUE)[[1]]
  labels<-datamatic$contrasts_labels[[fromb64(pieces[[1]])]]
  k<-suppressWarnings(as.integer(pieces[[2]]))
  if (is.null(labels) || length(pieces)<2 || is.na(k) || k>length(labels))
    return(NULL)
  labels[[k]]
}

# What a term compares, for the Effect column of the regression tables: a factor
# contrast shows its comparison (e.g. "2 - 1"), any other variable its name.
# In interactions each comparison is put in parentheses: "x1:(2 - 1)".
.term_comparison<-function(term, datamatic) {
  parts<-vapply(term, function(part) {
    comparison<-contrast_comparison(part,datamatic)
    if (is.null(comparison))
      return(gsub(FACTOR_SYMBOL,"",part,fixed=TRUE))
    if (length(term)>1) paste0("(",comparison,")") else comparison
  }, character(1), USE.NAMES=FALSE)
  jmvcore::stringifyTerm(parts)
}

# Return layouts, not jamovi tables; SmartArray dispatches each child payload.
# Names (`source`, shown with showRealNames) is the contrast variable, e.g.
# "groups_a1"; Effect (`label` / `rhs`) is what it compares, e.g. "2 - 1".
regression_layout <- function(dep, terms, estimator, datamatic, total = FALSE) {
    rows <- lapply(estimator$expandTerms(terms), function(term) {
        name <- .term_label(term)
        effect <- .term_comparison(term, datamatic)
        if (total) list(source = name, label = effect) else list(lhs = dep, source = name, rhs = effect)
    })
    anova <- if (total) list(dep = dep) else list(lhs = dep)
    list(anova = list(anova), regression = rows)
}

# Preserve the existing B64/factor-aware fitting, returning values for SmartTabs.
regression_values <- function(dep, terms, estimator, datamatic, data, ciWidth, total = FALSE) {
    formula <- estimator$modelFormula64(dep, terms)
    tables <- .regression_tables(formula, data, ciWidth)
    layout <- regression_layout(dep, terms, estimator, datamatic, total)
    rows <- lapply(seq_along(layout$regression), function(index) {
        c(layout$regression[[index]], as.list(tables[[1]][index, , drop = FALSE]))
    })
    list(anova = list(c(layout$anova[[1]], tables[[2]])), regression = rows)
}
