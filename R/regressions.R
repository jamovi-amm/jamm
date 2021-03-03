
.regression_table<-function(modelFormula,data,names64,ciWidth) {
     
     estimates <- try({
       model<-stats::lm(modelFormula, data=data)
       smr<-summary(model)
       res<-as.data.frame(smr$coefficients)
       res$df<-smr$df[2]
       ci<-confint(model,level = ciWidth)
       res<-cbind(res,ci)
       colnames(res)<-c("estimate","se","t","p","df","cilow","cihig")
       res
        })
     if (jmvcore::isError(estimates)) {
           msg<-jmvcore::extractErrorMessage(estimates)
           msg<-names64$translate(msg)
           jmvcore::reject(msg, code='error')
     }

     toscale<-all.vars(as.formula(modelFormula))
     zdata<-as.data.frame(scale(data[,toscale]))
     beta<-stats::lm(modelFormula, data=zdata)
     estimates$beta<-beta$coefficients
     estimates[-1,]
}

.regression_tables<-function(modelFormula,data,names64,ciWidth) {
  
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
    msg<-names64$translate(msg)
    jmvcore::reject(msg, code='error')
  }
  
  toscale<-all.vars(as.formula(modelFormula))
  zdata<-as.data.frame(scale(data[,toscale]))
  beta<-stats::lm(modelFormula, data=zdata)
  estimates$beta<-beta$coefficients
  list(estimates[-1,],anov)
}


regressions.init<-function(infos,data, options, results, names64) {

#  results$regressions$setVisible(TRUE)
  results$regressions$setTitle("Regressions Results")

  #initial model
  agroup<-results$regressions$overall
  atable<-agroup$regression
  mod<-infos$original_fullmodel
  atable$setTitle(paste("Total effects predicting:",names64$nicenames(mod$dep)))
  amodel<-remove_a_from_b(infos$mediators,mod$ind)
  anames<-names64$nicenames(amodel)
  alabels<-names64$nicelabels(amodel)
  for (i in seq_along(amodel)) {
    term<-jmvcore::stringifyTerm(anames[[i]])
    label<-jmvcore::stringifyTerm(alabels[[i]])
    atable$addRow(rowKey=paste0(amodel[i]),list("source"=term,"label"=label))
  }
  
    
  ### mediators model
  medgroups<-results$regressions$mediator_regressions
  if (length(infos$mediators)>1)
    medgroups$setTitle("Mediators Models")
  
  for (mod in infos$original_medmodels) {
    agroup<-medgroups$addItem(key=mod$dep)
    agroup$setTitle(paste("Dependent variable:",names64$nicenames(mod$dep)))
    amodel<-mod$ind
    anames<-names64$nicenames(amodel)
    alabels<-names64$nicelabels(amodel)
    atable<-agroup$regression
    for (i in seq_along(amodel)) {
      term<-jmvcore::stringifyTerm(anames[[i]])
      label<-jmvcore::stringifyTerm(alabels[[i]])
      atable$addRow(rowKey=paste0(amodel[i]),list("source"=term,"label"=label))
    }
    
    
  }
  
  
  ### full model  
  fullgroup<-results$regressions$full
  fullrtable<-fullgroup$regression  
  fullrtable$setTitle(paste("Full model predicting",names64$nicenames(infos$original_fullmodel$dep)))
  fullmodel<-infos$original_fullmodel$ind
  fullnames<-names64$nicenames(fullmodel)
  fulllabels<-names64$nicelabels(fullmodel)

  for (i in seq_along(fullmodel)) {
    term<-jmvcore::stringifyTerm(fullnames[[i]])
    label<-jmvcore::stringifyTerm(fulllabels[[i]])
    fullrtable$addRow(rowKey=paste0(fullmodel[i]),list("source"=term,"label"=label))
  }

}

regressions.results<-function(infos,data, options, results, names64) {

  ciWidth<-options$ciWidth/100

  ## initial model
  agroup<-results$regressions$overall
  atable<-agroup$regression
  # here we have to deduce the total model because infos does not compute it
  # it is the full model without mediators
  mod<-infos$original_fullmodel
  modelterms<-remove_a_from_b(infos$mediators,mod$ind)
  modelFormula<-jmvcore::composeFormula(mod$dep,modelterms)
  tables<-.regression_tables(modelFormula,data,names64,ciWidth) 
  rtable<-tables[[1]] 
  atable<-tables[[2]]

  outtable<-agroup$regression
  for (i in seq_along(modelterms)) {
    outtable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
  }
  outtable<-agroup$anova
      outtable$addRow(rowKey=1,atable)
  
  
    
  ### mediators model
  medgroups<-results$regressions$mediator_regressions
  
  for (mod in infos$original_medmodels) {
    agroup<-medgroups$get(key=mod$dep)
    modelterms<-mod$ind
    modelFormula<-jmvcore::composeFormula(mod$dep,modelterms)
    tables<-.regression_tables(modelFormula,data,names64,ciWidth)
    rtable<-tables[[1]] 
    atable<-tables[[2]]
    
    outtable<-agroup$regression
    for (i in seq_along(modelterms)) {
      outtable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
    }
    outtable<-agroup$anova
    outtable$addRow(rowKey=1,atable)
    
    
  }  
  
  ## full models  
  modelterms<-infos$original_fullmodel$ind
  modelFormula<-jmvcore::composeFormula(infos$original_fullmodel$dep,modelterms)
  tables<-.regression_tables(modelFormula,data,names64,ciWidth) 
  rtable<-tables[[1]] 
  atable<-tables[[2]]
  
  fullgroup<-results$regressions$full
  outtable<-fullgroup$regression
  for (i in seq_along(modelterms)) {
    outtable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
  }
  outtable<-fullgroup$anova
  outtable$addRow(rowKey=1,atable)
  
}
  
  
