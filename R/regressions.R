
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


regressions.init<-function(infos,data, options, results, names64) {

  results$regressions$setTitle("Regressions Results")

  #initial model
  atable<-results$regressions$overall
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
  medtables<-results$regressions$mediator_regressions
  if (length(infos$mediators)>1)
    medtables$setTitle("Mediators Models")
  
  for (mod in infos$original_medmodels) {
    atable<-medtables$addItem(key=mod$dep)
    atable$setTitle(paste("Dependent variable:",names64$nicenames(mod$dep)))
    amodel<-mod$ind
    anames<-names64$nicenames(amodel)
    alabels<-names64$nicelabels(amodel)
    for (i in seq_along(amodel)) {
      term<-jmvcore::stringifyTerm(anames[[i]])
      label<-jmvcore::stringifyTerm(alabels[[i]])
      atable$addRow(rowKey=paste0(amodel[i]),list("source"=term,"label"=label))
    }
    
    
  }
  
  
  ### full model  
  fulltable<-results$regressions$full
  fulltable$setTitle(paste("Full model predicting",names64$nicenames(infos$original_fullmodel$dep)))
  fullmodel<-infos$original_fullmodel$ind
  fullnames<-names64$nicenames(fullmodel)
  fulllabels<-names64$nicelabels(fullmodel)

  for (i in seq_along(fullmodel)) {
    term<-jmvcore::stringifyTerm(fullnames[[i]])
    label<-jmvcore::stringifyTerm(fulllabels[[i]])
    fulltable$addRow(rowKey=paste0(fullmodel[i]),list("source"=term,"label"=label))
  }

}

regressions.results<-function(infos,data, options, results, names64) {

  ciWidth<-options$ciWidth/100

  ## initial model
  atable<-results$regressions$overall
  # here we have to deduce the total model because infos does not compute it
  # it is the full model without mediators
  mod<-infos$original_fullmodel
  modelterms<-remove_a_from_b(infos$mediators,mod$ind)
  modelFormula<-jmvcore::composeFormula(mod$dep,modelterms)
  rtable<-.regression_table(modelFormula,data,names64,ciWidth) 
  for (i in seq_along(modelterms)) {
    atable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
  }
  
    
  ### mediators model
  medtables<-results$regressions$mediator_regressions
  
  for (mod in infos$original_medmodels) {
    atable<-medtables$get(key=mod$dep)
    modelterms<-mod$ind
    modelFormula<-jmvcore::composeFormula(mod$dep,modelterms)
    rtable<-.regression_table(modelFormula,data,names64,ciWidth) 
    for (i in seq_along(modelterms)) {
      atable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
    }
  }  
  
  ## full models  
  jtable<-results$regressions$full
  modelterms<-infos$original_fullmodel$ind
  modelFormula<-jmvcore::composeFormula(infos$original_fullmodel$dep,modelterms)
  rtable<-.regression_table(modelFormula,data,names64,ciWidth) 
  for (i in seq_along(modelterms))
      jtable$setRow(rowKey=paste0(modelterms[i]),rtable[i,])
}
