library(R6)
smartMediation <- R6Class("smartMediation",
                public=list(
                independents=NULL,
                mediators=NULL,
                moderators=NULL,
                moderatorsTerms=NULL,
                dep=NULL,
                vars=NULL,
                nvars=NULL,
                nmeds=NULL,
                ninds=NULL,
                medmodels=NULL,
                fullmodel=NULL,
                original_medmodels=NULL,
                original_fullmodel=NULL,
                M=NULL,
                isImpossible=FALSE,
                impossibles=NULL,
                ieffects=NULL,
                mod_ieffects=NULL,
                totaleffects=NULL,
                ipaths=NULL,
                messages=list(),
                initialize = function(medModels=NULL,fullModel=NULL,moderators=NULL) {
                  
                                 if (!is.something(medModels))
                                     medModels<-list(list(dep=NULL,ind=NULL))
                                 if (!is.something(fullModel))
                                     fullModel<-list(dep=NULL,ind=NULL)

                                 self$original_medmodels<-medModels
                                 self$original_fullmodel<-fullModel
                                 self$moderators<-flat_list(moderators)
                                 private$.moderators<-moderators
                                 self$init()
                },
                init=function() {
                  private$handleModerators()
                  private$fixVariables()
                  private$suggested()
                  private$required()
                  self$M<-.toMatrix(self$M,self$medmodels,self$fullmodel,"P")  
                  private$indirects()
                  },
                isEstimable=function() {
                  (!self$isImpossible && !self$hasRequired())
                },
                
                hasRequired=function() {
                  (any(self$M=="R") || length(grep(".....",self$vars,fixed = T)>0))
                },
                hasSuggested=function() {
                  any(self$M=="S")
                } ,
                hasModerators=function() {
                  (length(self$moderators)>0)
                } ,
                
                moderatedPaths=function() {
                  paths<-list()
                  for(mod in names(self$ipaths))
                     for (coord in self$ipaths[[mod]]) {
                       if (is.something(coord$from) && coord$from!=mod) {
                        paths[[mod]]=concat(paths[[mod]],list(from=which(self$vars==coord$from),to=which(self$vars==coord$to)))   
                       }}
                  paths

                },
                print=function() {
                  add<-""
                  if (self$hasRequired())
                    add<-" (R)"
                  .cat<-function(what) cat(paste0("\n",what,add,"\n"))
                  
                  .cat("N variables")
                  cat(self$nvars)
                  .cat("Variables")
                  cat(self$vars)
                  .cat("Independent variables")
                  cat(self$independents)
                  .cat("Mediators")
                  cat(self$mediators)
                  .cat("Moderators")
                  print(self$moderators)
                  .cat("Mediators Models")
                  print(self$medmodels)
                  .cat("Full Models")
                  print(self$fullmodel)
                  .cat("Original Mediators Models")
                  print(self$original_medmodels)
                  .cat("Original Full Models")
                  print(self$original_fullmodel)
                  
                  cat("\nPaths\n")
                  print(self$M)
                  cat("\nIs impossible?\n")
                  cat(self$isImpossible)
                  cat("\nHas required\n")
                  cat(self$hasRequired())
                  cat("\nHas suggested\n")
                  cat(self$hasSuggested())
                  cat("\n")
                  if (self$isImpossible) {
                    cat("\nImpossible paths\n")
                    print(self$impossibles)
                  }
                  else {
                    cat("\n Indirect Effects\n")
                    print(self$ieffects)
                  }
                }
                
                ), #end of public
                private = list(
                   .moderators=NULL,
                   fixVariables=function() {
                    vnames<-private$extractVariables()
                    self$mediators<-fill.if(!is.something(vnames$mediators),"M.....",vnames$mediators)
                    self$independents<-fill.if(!is.something(vnames$independents),"X.....",vnames$independents)
                    self$dep<-fill.if(!is.something(vnames$dep),"Y.....",vnames$dep)
                    self$vars<-c(self$independents,self$mediators,self$dep)
                    self$nvars<-length(self$vars)
                    self$moderatorsTerms<-lolapply(private$.moderators,self$mediators)
                   },
                   extractVariables=function() {
                     ### we harvest the mediators names ###
                     mediators<-flat_named_list(self$medmodels,"dep")

                     ### we harvest the predictors names ###
                     forms<-concat(self$medmodels,self$fullmodel)
                     vars<-flat_named_list(forms,"ind")
                     
                     ### remove moderators #####
                     ### we harvest the dependent variable ###
                     dep<-self$original_fullmodel$dep
                     
                     return(list(dep=dep,mediators=mediators,independents=setdiff(vars,mediators)))
                   },
                   handleModerators=function() {
                     ipaths<-list()
                     meds<-self$original_medmodels
                     full<-self$original_fullmodel
                     models<-concat(meds,full)
                     for (mod in self$moderators) 
                        for (i in seq_along(models)) {
                          where=findTerms(mod,models[[i]]$ind)
                          for (term in models[[i]]$ind[where]) {
                            if (length(term)==1)
                               ipaths[[mod]]=concat(ipaths[[mod]],list(from=mod,to=models[[i]]$dep, type="P"))
                            else {
                               ipaths[[mod]]=concat(ipaths[[mod]],list(from=term[!(term %in% self$moderators)],to=models[[i]]$dep,type="P"))
                            }
                          }
                          models[[i]]$ind<-models[[i]]$ind[!where]
                        }
                    # now that we removed the moderators, we treat other interactions
                    # as standard variables for plotting by forcing the interaction in a x:z format

                      for (i in seq_along(models))
                         for (j in seq_along(models[[i]]$ind)) 
                           models[[i]]$ind[[j]]<-paste0(models[[i]]$ind[[j]],collapse =":")

                    self$medmodels<-models[-length(models)]
                    self$fullmodel<-models[[length(models)]]
                    
                    self$ipaths=ipaths
                   },
                   
                   required=function() {
                      meds<-self$medmodels
                      full<-self$fullmodel
                      
                      inds<-self$independents
                      mediators<-self$mediators
                    for (i in seq_along(meds)) {
                           if (is.null(meds[[i]]$ind)) meds[[i]]$ind<-inds
                           if (is.null(meds[[i]]$dep)) meds[[i]]$dep<-mediators[i]
                           
                    }
                    if (!any(inds %in% full$ind)) full$ind=c(mediators,inds)
                    
                    full$dep=self$dep
                   
                    out<-which(!(mediators %in% full$ind))
                    if (length(out)>0) {
                         outnames<-mediators[out]
                         full$ind<-c(full$ind,outnames)
                    }
                    for (i in 1:length(meds)) {
                      m<-meds[[i]]
                      if (is.null(unlist(m$ind)))
                         next()
                      absent<-!(m$ind %in% full$ind)
                      nf<-sum(as.numeric(absent))
                      np<-sum(as.numeric(!absent))
                      if (nf>0 && np==0) {
                        outnames<-m$ind[which(absent)]
                        meds[[i]]$ind<-c(full$ind,outnames)
                      }
                      if (length(m$ind)==0)
                        meds[[i]][["ind"]]<-inds
                    }
                    self$M<-.toMatrix(self$M,meds,full,"R")  
                  },
                  suggested=function() {
                      inds<- self$independents
                      meds<-self$medmodels
                      full<-self$fullmodel
                      for (i in seq_along(meds)) {
                        meds[[i]][['ind']]<-unique(c(unlist(meds[[i]]['ind']),unlist(inds)))
                      }
                      full$ind<-unique(c(inds,self$mediators))
                      full$dep<-self$dep
                      M<-matrix("0",ncol=length(self$vars),nrow=length(self$vars))
                      rownames(M)<-colnames(M)<-self$vars
                      self$M<-.toMatrix(M,meds,full,"S")
                    },
                    indirects=function() {

                      if (self$hasRequired()) {
                        self$isImpossible<-FALSE
                        self$impossibles<-NULL
                        self$ieffects<-NULL
                        return()
                      }
                        
                      isImpossible<-FALSE
                      impossibles <- NULL
                      lists<-.getPathsPairs(self)
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
                              if (m[2,i]==self$dep) {
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
                                impossibles<-unique(lapply(left,function(l) {
                                  l[!(l %in% self$independents)]
                                  }))
                                doit<-FALSE
                              }
                            }
                          }
                        }
                        mod_ieffects<-indeff
                        for (j in seq_along(self$moderators)) 
                          for (i in seq_along(indeff)) {
                           where<-grep(self$mediators[j],indeff[[i]],fixed=T)
                           if (is.something(where) && is.something(self$moderators[[j]]))
                             mod_ieffects[[i]]<-c(indeff[[i]],paste("..mod..",self$moderators[[j]]))
                             
                        }
                        self$isImpossible<-isImpossible
                        self$impossibles<-impossibles
                        self$mod_ieffects<-mod_ieffects
                        self$ieffects<-indeff
                        totaleffects<-list()
                        for (ie in indeff) {
                          value<-c(ie[[1]],ie[[length(ie)]])
                          key<-paste0(value,collapse = "_")
                          totaleffects[[key]]<-value
                        }
                        self$totaleffects<-totaleffects
                    }
                    
                )  # end of private
) # end of class
                  
### helper functions

.toMatrix<-function(aM,meds,full,val="") {
  models<-meds
  models[[length(meds)+1]]<-full
   vars<-colnames(aM)
   for (f in models) {
    d<-which(vars==f$dep)
    i<-which(vars %in% f$ind)
    aM[d,i]<-val
  }
  aM
}


.getPathsPairs<-function(infos) {
  
  #### first, we get paths from x to m
  xList<-list()
  k<-0
  meds<-infos$original_medmodels
  for (i in seq_along(meds))
      for (j in seq_along(meds[[i]]$ind))
          meds[[i]]$ind[[j]]<-paste(meds[[i]]$ind[[j]], collapse = ":")

  for (v in infos$independents) {
    where<-grep(v,meds,fixed = T)
    for (i in where) {
      k<-k+1
      xList[[k]]<-list(from=v,to=infos$mediators[i])
    }
  }
  #### then we get all paths  to m or y
  models<-meds
  models[[length(meds)+1]]<-infos$original_fullmodel
  mList<-list()
  k<-0
  for (v in infos$mediators) {
    where<-grep(v,models,fixed = T)
    for (i in where) {
      if (v!=models[[i]][["dep"]]) {
        k<-k+1
        mList[[k]]<-list(from=v,to=models[[i]][["dep"]])
      }
    }
  }
  return(list(xList=xList,mList=mList))
  
}

### toberemoved ####
.handleInteractions<-function(infos) {
  
  if (is.null(infos$moderators)) 
    return(list(infos=infos,iM=NULL))
  terms<-jmvcore::decomposeTerms(colnames(infos$M))
  where<-findTerms(infos$moderators,terms,1)
  oM<-infos$M
  ## fix the Main matrix by removing the interaction terms involving the moderators ####
  M=oM[!where,!where]
  infos$nvars<-nrow(M)
  namesleft<-rownames(M)
  infos$vars<-namesleft
  infos$mediators<-namesleft[(length(namesleft)-length(infos$mediators)):(length(namesleft)-1)]
  infos$independents<-namesleft[1:(length(namesleft)-length(infos$mediators)-1)]
  infos$M<-M
  
  # extract moderated paths
  
  ipaths<-list()
  k<-0
  where<-findTerms(infos$moderators,terms,2)
  iM=matrix(oM[,where],nrow=nrow(oM))
  rownames(iM)<-rownames(oM)
  colnames(iM)<-colnames(oM)[where]  

  ## to draw interactions, we need to know moderator name, the index in the nex M of the two variables whose
  ## path is moderated by the moderator, and if is a required, suggested or actual path.
  ## here is the loop required.
  for (name in colnames(iM)) {
    term<-jmvcore::decomposeTerm(name)
    modwhere<-findTerms(infos$moderators,term)
    mod<-term[modwhere]
    aterm<-jmvcore::composeTerm(term[!modwhere])
    one<-iM[iM[,name]!="0",name]
    for (i in seq_along(one)) ipaths[[mod]]=concat(ipaths[[mod]],list(mod=mod,from=which(namesleft==aterm),to=which(namesleft==names(one)[i]),type=one[[i]]))
  }
  list(infos=infos,ipaths=ipaths)
}


