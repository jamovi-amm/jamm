##### this will understand what kind of mediational model we have
##### which are the mediators, etc. etc. 
##### it produces a structure of class "smartMediation+ModelType"
##### from now, only modelType="GLM"

smart.understand<-function(meds,full,modelType="GLM") {
  infos<-.understand(meds,full)
  .req<-.required(infos)
  infos$required<-.req$infos
  infos$hasRequired<-.req$hasRequired
  if (infos$hasRequired) {
    infos$indeff<-NULL
    infos$isImpossible<-FALSE
    infos$impossibles<-NULL
    
  } else {
      ie<-smart.computeIndirect(infos)
      infos$isImpossible<-ie$isImpossible
      infos$impossibles<-ie$impossibles
      infos$indeff<-ie$indeff
  }
  infos$suggested<-.suggested(infos)
  infos$ori_medmodels<-meds
  infos$ori_full<-full
  return(infos)
}


.understand<-function(meds,full,modelType="GLM") {

  if (!is.something(meds))
    meds<-list(list(dep=NULL,ind=NULL))
  if (!is.something(full))
    full<-list(dep=NULL,ind=NULL)

  .meds<-.cleanMeds(meds)
  .full<-.cleanFull(full)

  forms<-.meds
  mediators<-NULL
  checkmed<-try({
    mediators<-unlist(unique(sapply(.meds,function(a) a$dep)))
  })
  dep<-NULL
  .var<-NULL
  checkind<-try({
     forms[[length(forms)+1]]<-.full
     .vars<-unique(unlist(sapply(forms,function(a) a$ind)))
     dep<-full$dep
  })
  indeps<-setdiff(.vars,mediators)
  vars<-c(indeps,mediators,dep)
  nvars<-length(vars)
  nmeds<-length(mediators)
  ninds<-length(indeps)
  
 
  structure(list(dep=dep,
                 mediators=mediators,
                 independents=indeps,
                 vars=vars,
                 nvars=nvars,
                 nmeds=nmeds,
                 ninds=ninds,
                 nmodels=length(forms),
                 models=forms),
                 class=c(paste0("smartMediation",modelType),"smartMediation"))
}


.cleanMeds<-function(aList) {

  for (j in seq_along(aList)) {
     terms<-aList[[j]]
     noint<- grep(":",terms$ind,fixed=T,value=FALSE,invert = TRUE)
     ind<-aList[[j]]$ind[noint]
     ind<-setdiff(ind,terms$dep)
     aList[[j]]$ind<-ind
   }
   aList<-lapply(aList, function(m) list(dep=m$dep,ind=setdiff(m$ind,m$dep)))
  
   return(aList)      
}
  


.cleanFull<-function(full) {
  
    noint<- grep(":",full$ind,fixed=T,value=FALSE,invert = TRUE)
    full$ind<-full$ind[noint]
    full$ind<-setdiff(full$ind,full$dep)
  
  return(full)
}


.requiredVariables<-function(infos) {

  mods<-infos$models
  mediators<-fill.if(!is.something(infos$mediators),"M.....",infos$mediators)
  inds<-fill.if(!is.something(infos$independents),"X.....",infos$independents)
  dep<-fill.if(!is.something(infos$dep),"Y.....",infos$dep)
    
    meds<-mods[-length(mods)]
    full<-mods[[length(mods)]]
    for (i in seq_along(meds)) {
      if (is.null(meds[[i]]$dep)) meds[[i]]$dep<-mediators[i]
      if (is.null(meds[[i]]$ind)) meds[[i]]$ind<-inds
    }
    if (!any(inds %in% full$ind)) full$ind=c(mediators,inds)
    full$dep=dep
  .understand(meds,full)

}

.required<-function(infos) {
  
  infos<-.requiredVariables(infos)
  mediators<-infos$mediators
  hasRequired<-is.something(grep(".....",infos$vars,fixed = T))
  full<-infos$models[[length(infos$models)]]
  
  out<-which(!(mediators %in% full$ind))
  if (length(out)>0) {
    outnames<-infos$mediators[out]
    infos$models[[infos$nmodels]][["ind"]]<-c(full$ind,outnames)
  }
  
  for (i in 1:(infos$nmodels-1)) {
    m<-infos$models[[i]]
    absent<-!(m$ind %in% full$ind)
    nf<-sum(as.numeric(absent))
    np<-sum(as.numeric(!absent))
    if (nf>0 & np==0) {
      outnames<-m$ind[which(absent)]
      infos$models[[infos$nmodels]][["ind"]]<-c(full$ind,outnames)
    }
    if (length(m$ind)==0)
      infos$models[[i]][["ind"]]<-infos$independents
  }
  list(infos=infos,hasRequired=hasRequired)  
}


.suggested<-function(infos) {
  models<-infos$required$models
  inds<- infos$required$independents
  for (i in seq_len(infos$required$nmodels)) 
    models[[i]][['ind']]<-unique(c(unlist(models[[i]]['ind']),unlist(inds)))
 
  .understand(models[1:(infos$required$nmodels-1)],models[[infos$required$nmodels]])
}


######## function to compute all possible indirect effects #####
smart.computeIndirect<- function(x,...) UseMethod(".computeIndirect")

.computeIndirect.smartMediation<-function(infos) {
  
     isImpossible<-FALSE
  
     lists<-.getPathsPairs(infos)
     mList<-lists$mList
     xList<-lists$xList
     k<-0
     m<-matrix(sapply(mList,function(a) a),nrow = 2)     
     doit<-TRUE
     left<-xList
     indeff<-list()
     while (doit) {
       original<-left
       left<-list()
       nleft<-0
       for (l in original) {
         l<-unlist(l)
         llength<-length(l)
         where<-grep(l[llength],m[1,],fixed=T)
         for (i in where) {
         if (m[2,i]==infos$dep) {
           k<-k+1
           indeff[[k]]<-unique(unlist(c(l,m[,i])))
         }
         else {
           nleft<-nleft+1
           left[[nleft]]<-unique(unlist(c(l,m[,i])))
         }
         }
       }
       if (!is.something(left))
         doit<-FALSE
       else {
         if (length(left)>1) {
            comb<-combn(length(left),2)
            test<-any(sapply(1:ncol(comb), function(i) {
             setequal(left[[comb[1,i]]],left[[comb[2,i]]])
            }))
             if (test) {
                   indeff<-NULL
                   isImpossible<-TRUE
                   doit<-FALSE
             }
         }
       }
     }
     return(list(isImpossible=isImpossible,
                 impossibles=left,
                 indeff=indeff))
     
     
} # end of computeIndirect()

.getPathsPairs<-function(infos) {
  
  #### first, we get paths from x to m
  xList<-list()
  k<-0
  meds<-infos$models[1:(infos$nmodels-1)]
  for (v in infos$independents) {
    where<-grep(v,meds,fixed = T)
    for (i in where) {
      k<-k+1
      xList[[k]]<-list(from=v,to=infos$mediators[i])
    }
  }
  #### then we get all paths  to m or y
  
  mList<-list()
  k<-0
  for (v in infos$mediators) {
    where<-grep(v,infos$models,fixed = T)
    for (i in where) {
      if (v!=infos$models[[i]][["dep"]]) {
        k<-k+1
        mList[[k]]<-list(from=v,to=infos$models[[i]][["dep"]])
      }
    }
  }
  return(list(xList=xList,mList=mList))
  
}


