
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

### expandTerms() marks factor dummies as "var<FACTOR_SYMBOL>level" (see Syntax$.factorlist());
### strip that back to the "var<level>" convention used elsewhere (e.g. models$contrastCodeTable)
### for a readable row label.
.term_label<-function(term) {
  jmvcore::stringifyTerm(gsub(FACTOR_SYMBOL,"",term,fixed=TRUE))
}


### Declares the (empty) rows for the "individual regressions" tables.
### Filled with numbers later by regressions.results(), once the data are estimated.
### Works off self$options (dep/mediators/mediatorsTerms/modelTerms) plus lav_machine
### (the Estimate/Syntax instance), which is used to expand any factor term into one
### row per contrast dummy - matching what stats::lm() will actually produce.
regressions.init<-function(options, results, lav_machine) {

  if (!("regression" %in% options$tableOptions))
    return()

  results$regressions$setTitle("Regressions Results")

  dep<-options$dep
  mediators<-options$mediators
  modelTerms<-options$modelTerms
  mediatorsTerms<-options$mediatorsTerms

  ## "total effects" model: the full model with any mediator-related term removed
  totalterms<-Filter(function(term) length(intersect(mediators,term))==0, modelTerms)
  totalterms<-lav_machine$expandTerms(totalterms)

  agroup<-results$regressions$overall
  atable<-agroup$regression
  atable$setTitle(paste("Total effects predicting:",dep))
  for (term in totalterms) {
    key<-paste(term,collapse=":")
    label<-.term_label(term)
    atable$addRow(rowKey=key,list(source=label,label=label))
  }

  ### mediators models
  medgroups<-results$regressions$mediator_regressions
  if (length(mediators)>1)
    medgroups$setTitle("Mediators Models")

  for (i in seq_along(mediators)) {
    med<-mediators[i]
    terms_i<-lav_machine$expandTerms(mediatorsTerms[[i]])
    agroup<-medgroups$addItem(key=med)
    agroup$setTitle(paste("Dependent variable:",med))
    atable<-agroup$regression
    for (term in terms_i) {
      key<-paste(term,collapse=":")
      label<-.term_label(term)
      atable$addRow(rowKey=key,list(lhs=med,rhs=label))
    }
  }

  ### full model
  fullterms<-lav_machine$expandTerms(modelTerms)
  fullgroup<-results$regressions$full
  fullrtable<-fullgroup$regression
  fullrtable$setTitle(paste("Full model predicting",dep))
  for (term in fullterms) {
    key<-paste(term,collapse=":")
    label<-.term_label(term)
    fullrtable$addRow(rowKey=key,list(lhs=dep,rhs=label))
  }

}

### Fills the "individual regressions" tables declared by regressions.init().
### `data` must already be the b64-renamed, cleaned data frame produced by
### Datamatic$cleandata() (same data the main mediation model is estimated on).
### `lav_machine` builds the B64/factor-dummy-aware formulas so the fit is
### consistent with the main mediation model.
regressions.results<-function(options, data, results, lav_machine) {

  if (!("regression" %in% options$tableOptions))
    return()

  ciWidth<-options$ciWidth/100

  dep<-options$dep
  mediators<-options$mediators
  modelTerms<-options$modelTerms
  mediatorsTerms<-options$mediatorsTerms

  ## total effects
  totalterms<-Filter(function(term) length(intersect(mediators,term))==0, modelTerms)
  modelFormula<-lav_machine$modelFormula64(dep,totalterms)
  totalterms<-lav_machine$expandTerms(totalterms)
  tables<-.regression_tables(modelFormula,data,ciWidth)
  rtable<-tables[[1]]
  anov<-tables[[2]]
  anov$dep<-dep

  agroup<-results$regressions$overall
  outtable<-agroup$regression
  for (i in seq_along(totalterms)) {
    key<-paste(totalterms[[i]],collapse=":")
    outtable$setRow(rowKey=key,rtable[i,])
  }
  agroup$anova$addRow(rowKey=1,anov)


  ### mediators models
  medgroups<-results$regressions$mediator_regressions

  for (i in seq_along(mediators)) {
    med<-mediators[i]
    terms_i<-mediatorsTerms[[i]]
    modelFormula<-lav_machine$modelFormula64(med,terms_i)
    terms_i<-lav_machine$expandTerms(terms_i)
    tables<-.regression_tables(modelFormula,data,ciWidth)
    rtable<-tables[[1]]
    anov<-tables[[2]]
    anov$lhs<-med

    agroup<-medgroups$get(key=med)
    outtable<-agroup$regression
    for (j in seq_along(terms_i)) {
      key<-paste(terms_i[[j]],collapse=":")
      row<-rtable[j,]
      row$lhs<-med
      row$rhs<-.term_label(terms_i[[j]])
      outtable$setRow(rowKey=key,row)
    }
    agroup$anova$addRow(rowKey=1,anov)
  }

  ## full model
  modelFormula<-lav_machine$modelFormula64(dep,modelTerms)
  fullterms<-lav_machine$expandTerms(modelTerms)
  tables<-.regression_tables(modelFormula,data,ciWidth)
  rtable<-tables[[1]]
  anov<-tables[[2]]
  anov$lhs<-dep

  fullgroup<-results$regressions$full
  outtable<-fullgroup$regression
  for (i in seq_along(fullterms)) {
    key<-paste(fullterms[[i]],collapse=":")
    row<-rtable[i,]
    row$lhs<-dep
    row$rhs<-.term_label(fullterms[[i]])
    outtable$setRow(rowKey=key,row)
  }
  fullgroup$anova$addRow(rowKey=1,anov)

}
