

jmf.mediationSummary <-
  function(infos,
           data,
           se,
           level,
           boot.ci=NULL,
           bootN = 1000,
           missing="listwise") {
    models <- infos$original_medmodels
    models[[length(models) + 1]] <- infos$original_fullmodel
    formulas <- .modelFormulas(models)
    ok<-unlist(lapply(formulas, function(f){
       class(tryCatch(as.formula(f), error=function(e) FALSE))=="formula"
    }))
    formulas<-formulas[ok]
    lavformula <- paste(formulas, collapse = " ; ")
    
    ## correlates the parallel mediators
    
    if (length(infos$mediators)>1) {
      MM<-infos$M[infos$mediators,infos$mediators]
      diag(MM)<-"NULL"
      pair<-list()
      for (i in 1:dim(MM)[1]) 
          for (j in 1:dim(MM)[1])
             if (MM[i,j]==MM[j,i] && MM[j,i]=="0") {
                 if (i>j) found<-c(colnames(MM)[i],rownames(MM)[j])
                 else found<-c(colnames(MM)[j],rownames(MM)[i])
                 pair[[length(pair)+1]]<-found
             }
      pair<-unique(pair)
      if (length(pair)>0) {
          medcor<-paste(lapply(pair, paste, collapse="~~"),collapse = " ; ")
          mark("Correlated terms", lapply(pair, jmvcore::fromB64))
          lavformula <- paste(lavformula,medcor, sep=" ; ",collapse =  " ;")
      } else
         mark("No correlated mediators")
    }

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
                      se = "standard",
                      missing=missing))
    if (jmvcore::isError(fit)) {
      msg <- jmvcore::extractErrorMessage(fit)
      if (is.something(grep("definite", msg)) || is.something(grep("S.inv", msg))) {
        jmvcore::reject(
          "The model cannot be estimated. Please check whether the independent variables are very highly correlated or the model is ill-defined"
        )
        mark("Error in summary",msg)
      }
      else
        jmvcore::reject(msg)
    }
    
    mtable<-lavaan::parameterestimates(fit, level = level, standardized = T)
    
    if (se=="bootstrap") {
      paral<-"multicore"
      if (.Platform$OS.type=="windows")
         paral<-"no"
      
      bfit<-try_hard(lavaan::sem(lavformula,
                  data = data,
                  se = "bootstrap",
                  missing=missing,
                  bootstrap = bootN,
                  parallel=paral))
    
     btable<-try_hard(lavaan::parameterestimates(
        bfit$obj,
        level = level,
        boot.ci.type = boot.ci
      ))
      if (!isFALSE(btable$error))
          stop(btable$error)
      else {
            mtable$ci.lower<-btable$obj$ci.lower
            mtable$ci.upper<-btable$obj$ci.upper
      }
    }
    attr(mtable,"fit")<-fit
    mtable

  }

jmf.mediationTotal <-
  function(infos,data,level,se,bootN=1000,boot.ci=NULL,missing="listwise") {
        .warning<-list()
        model <- infos$original_fullmodel
        meds<-infos$mediators
        ind<-lapply(model$ind, function(x) if(!any(x %in% meds)) x)
        model$ind<-ind
        .formula<-.modelFormula(model,"_t_")
        fit<-try(lavaan::sem(.formula,data = data,
                              likelihood = "wishart",
                             missing=missing,
                             se="standard"))
        if (jmvcore::isError(fit)) {
            msg <- jmvcore::extractErrorMessage(fit)
            if (is.something(grep("definite", msg)))
                    .warning<-append(.warning,"The total effect cannot be estimated. Please check whether the independent variables are very highly correlated or the model is ill-defined")
            else
                    .warning<-append(.warning,"The total effect cannot be estimated")
        }
        mtable<-lavaan::parameterestimates(fit, level = level, standardized = T)

        if (se=="bootstrap") {
          
          paral<-"multicore"
          if (.Platform$OS.type=="windows")
            paral<-"no"
          
          bfit<-lavaan::sem(.formula,data = data,
                      likelihood = "wishart",
                      missing=missing,
                      se="bootstrap",
                      bootstrap=bootN,
                      parallel=paral)
          
          btable<-lavaan::parameterestimates(
            bfit,
            level = level,
            boot.ci.type = boot.ci,
            standardized = T
          )
          mtable$ci.lower<-btable$ci.lower
          mtable$ci.upper<-btable$ci.upper
        }
        mtable
}


jmf.mediationTable <- function(
                infos,
                data,
                se = "standard",
                missing="listwise",
                level = 0.95, 
                boot.ci=NULL,
                bootN=1000) {
  
  if (se=="none") se<-"standard"
  if (boot.ci=="bca") boot.ci<-"bca.simple"
  models <- infos$original_medmodels
  models[[length(models) + 1]] <- infos$original_fullmodel
  ldata<-.make_var(models,data)
  params<-jmf.mediationSummary(infos,ldata, se=se,level=level,boot.ci=boot.ci,bootN=bootN,missing=missing)
  fit<-attr(params,"fit")
  params$model<-"med"
  totals<-jmf.mediationTotal(infos,ldata,se=se,level=level,boot.ci=boot.ci,bootN=bootN,missing=missing)
  totals$model<-"tot"
  mtable<-rbind(params,totals)
  attr(mtable,"fit")<-fit
  mtable
}

.modelFormulas <- function(models) {
  lapply(models, function(m) {
    .modelFormula(m)
  })
}

.modelFormula <- function(alist,sep="_") {
  dep <- alist$dep
  ind<-list()
  for (a in alist$ind)
      if (is.something(a))
         ind[[length(ind)+1]]<-a
  terms <- sapply(ind, function(a) {
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
          mod<-moderators[i]
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
