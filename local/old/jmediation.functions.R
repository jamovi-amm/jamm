

jmf.mediationSummary <-
  function(infos,
           data,
           se = "standard",
           bootN = 1000) {
    models <- infos$original_medmodels
    models[[length(models) + 1]] <- infos$original_fullmodel
    formulas <- .modelFormulas(models)
    lavformula <- paste(formulas, collapse = " ; ")
    ierecoded<-lapply(infos$ieffects, function(x) gsub(":","____",x))
    for (ie in ierecoded) {
      modifiers <- list()
      for (i in 1:(length(ie) - 1))
        modifiers[[i]] <- paste0(ie[i], "_", ie[i + 1])
      amodifier <- paste(modifiers, collapse = "*")
      amodifier <- paste(paste0(ie, collapse = "_"), amodifier, sep = ":=")
      lavformula <- paste(lavformula, amodifier, sep = ";")
    }
    fit <-
      try(lavaan::sem(lavformula,
                      data = data,
                      se = se,
                      bootstrap = bootN))
    if (jmvcore::isError(fit)) {
      msg <- jmvcore::extractErrorMessage(fit)
      if (is.something(grep("definite", msg)))
        jmvcore::reject(
#          "The model cannot be estimated. Please check whether the independent variables are very highly correlated or the model is ill-defined"
          msg
        )
      else
        jmvcore::reject(msg)
    }
    fit
  }

jmf.mediationTotal <-
  function(infos,data,level) {
        model <- infos$original_fullmodel
        meds<-infos$mediators
        ind<-lapply(model$ind, function(x) if(!any(x %in% meds)) x)
        model$ind<-ind[!(sapply(ind, is.null))]
        .formula<-.modelFormula(model,"_t_")
        fit<-try(lavaan::sem(.formula,data = data))
        if (jmvcore::isError(fit)) {
            msg <- jmvcore::extractErrorMessage(fit)
            if (is.something(grep("definite", msg)))
                    jmvcore::reject("The model cannot be estimated. Please check whether the independent variables are very highly correlated or the model is ill-defined")
      
        }
        table<-lavaan::parameterestimates(
          fit,
          level = level,
          standardized = T
        )
        table
}

jmf.mediationInference <- function(fit,
                                   level = .95,
                                   boot.ci = NULL) {
  if (is.null(boot.ci))
    mtable<-lavaan::parameterestimates(fit, level = level, standardized = T)
  else
    mtable<-lavaan::parameterestimates(
      fit,
      level = level,
      boot.ci.type = boot.ci,
      standardized = T
    )
}

jmf.mediationTable <- function(
                infos,
                data,
                se = "standard",
                level = 0.95, 
                boot.ci=NULL,
                bootN=100) {
  
  models <- infos$original_medmodels
  models[[length(models) + 1]] <- infos$original_fullmodel
  ldata<-.make_var(models,data)
  fit<-jmf.mediationSummary(infos,ldata, se=se,bootN=bootN)
  params<-jmf.mediationInference(fit,level=level,boot.ci =boot.ci)
  params$model<-"med"
  totals<-jmf.mediationTotal(infos,ldata,level)
  totals$model<-"tot"
  mtable<-rbind(params,totals)
  mtable
}

.modelFormulas <- function(models) {
  lapply(models, function(m) {
    .modelFormula(m)
  })
}

.modelFormula <- function(alist,sep="_") {
  dep <- alist$dep
  terms <- sapply(alist$ind, function(a) {
    if (length(a)>1)
      paste(paste0(paste0(a,collapse = "____"),sep,dep),paste0(a,collapse = "____"),sep="*")
    else
      paste(paste0(a, sep, dep), a, sep = " * ")
  })
  terms <- paste(terms, collapse = " + ")
  lformula <- paste(dep, "~", terms)
  return(lformula)
  
}

.prod<-function(data) {
  res<-data[,1]
  for (i in 2:ncol(data))
    res<-res*data[,i]
  res
}

.make_var<-function(models,data) {
  for(i in seq_along(models))
    for (j in seq_along(models[[i]]$ind)) {
      term<-unlist(models[[i]]$ind[j])
      if (length(term)>1) {
        name<-paste(term,collapse ="____")
        data[,name]<-.prod(data[,term])        
      }
    }
  data
}

expand.levels<-function(moderators,cov_condition) {
     labelsList<-list()
     for (i in seq_along(moderators)) {
          mod<-moderators
          labelsList[[mod]]<-cov_condition$labels(mod)
     }
     labelsList<-rev(labelsList)
     expand.grid(labelsList,stringsAsFactors = F)
}

expand.levels_numeric<-function(moderators,cov_condition) {
  
  labelsList<-list()
  for (i in seq_along(moderators)) {
    mod<-moderators[i]
    labelsList[[mod]]<-seq_along(cov_condition$labels(mod))
  }
  labelsList<-rev(labelsList)
  eg<-expand.grid(labelsList,stringsAsFactors = F)
  as.data.frame(eg)
}
