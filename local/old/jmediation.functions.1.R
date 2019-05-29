jmf.extractMediated<-function(infos,summaries) {
  
  for (ie in infos$ieffects) {
     ie<-ie[grep("..mod..",ie,fixed = T,invert=T)]
   
  }
  
}



jmf.modelSummaries<-function(infos,data) {

  models<-infos$original_medmodels
  models[[length(models)+1]]<-infos$original_fullmodel
  formulas<-.modelFormulas(models)  
  vars<-c(infos$mediators,infos$dep)
  ## estimate the models and extract coefficients
  ests<-lapply(formulas,lm,data=data)
  .cis<-lapply(ests,confint)
  .cis<-lapply(.cis,function(a) {
    colnames(a)<-c("cilow","cihig")
    a
  })
  cis<-.assignNames(.cis,vars)
  
  .sumrs<-lapply(lapply(ests,summary),function(a) a$coefficients)
  sumrs<-lapply(.sumrs,function(a) {
     colnames(a)<-c("estimate","se","z","p")
     a
     })
  mcc<-.assignNames(sumrs,vars)
  ### now we make the products and other info
  iecoefs<-list()
  for (i in seq_along(infos$ieffects)) {
    ie<-infos$ieffects[[i]]
    ie<-ie[grep("..mod..",ie,fixed=T,invert=T)]
    iecoef<-1
    scoef<-list()
    for (j in seq_len(length(ie)-1)) {
      from<-ie[j]
      to<-ie[j+1]
      scoef[[j]]<-list()
      onecis<-cis[[to]][from,]
      scoef[[j]]=c(mcc[[to]][from,],onecis)
      scoef[[j]]["p"]<-1-pnorm(scoef[[j]]["p"],lower.tail=FALSE)*2
      iecoef<-iecoef*mcc[[to]][from,"estimate"]
    }
    last=length(scoef)+1
    scoef[[last]]<-list()
    scoef[[last]]$estimate=iecoef
    scoef[[last]]$se=""
    iecoefs[[i]]<-scoef
    }
  
 return(iecoefs)
}

.modelFormulas<-function(models) {
  lapply(models, function(m) {
    .modelFormula(m)
  })
}


.modelFormula<-function(alist) {
  dep <- alist$dep
  terms<-sapply(alist$ind,identity)
  lformula<-jmvcore::constructFormula(dep=dep,terms) 
  return(lformula)
  
}

.stderr<-function(mod) {
  ss<-summary(mod)$coefficients
  nn<-rownames(ss)[-1]
  ss<-ss[rownames(ss)!="(Intercept)",2]
  names(ss)<-nn
  ss
}

.assignNames<-function(obj,objnames) {
  res<-list()
  for (i in seq_along(objnames)) {
     res[[objnames[i]]]<-obj[[i]]
  }
  
  res
}