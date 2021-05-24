library(R6)

names64 <- R6Class("names64",list(
                         .factors = list(),
                         .covs= list(),
                         .labels= list(),
                         .contrasts=list(),
                         addFactor = function(variable,levels) {
                           var64<-jmvcore::toB64(variable)
                           self$.factors[[var64]]<-variable
                           self$.contrasts[[var64]]<-list()
                           for(i in seq_along(levels[-1])) {
                             name64<-paste0(var64,i)
                             name<-paste0(variable,i)
                             self$.factors[[name64]]<-name
                             name<-paste0(var64,"_._._",i)
                             self$.contrasts[[var64]][i]<-name
                           }
                           ## this is for lme4 random names without intercept
                           for(a in levels) {
                             name64<-paste0(var64,a)
                             name<-paste0(variable,a)
                             self$.factors[[name64]]<-name
                           }
                           
                         },
                         addLabel=function(variable,labels) {
                           var64<-jmvcore::toB64(variable)
                           for (i in seq_along(labels)) {
                             name<-paste0(var64,"_._._",i)
                             self$.labels[[name]]<-labels[[i]]
                           }
                         },
                         nicelabels=function(obj) {
                           q<-sapply(obj,function(x) {
                             a<-unlist(strsplit(x,":",fixed = T))
                             b<-sapply(a, function(x) self$.onelabel(x))
                             paste(b,collapse = ":")    
                           })
                           return(q)
                         },
                         
                         addVar=function(variable) {
                           var64<-jmvcore::toB64(variable)
                           self$.covs[[var64]]<-variable
                         },
                         nicenames=function(obj) {
                                 sapply(obj,function(x) {
                                 a<-unlist(strsplit(x,":",fixed = T))
                                 b<-sapply(a, function(x) self$.onename(x))
                                 paste(b,collapse = ":")    
                                 })
                                 },
                         factorName=function(var64) {
                           x<-unlist(strsplit(var64,"_._._",fixed = T))
                           jmvcore::fromB64(x[1])
                         },
                         
                         
                         contrasts=function(var) {
                           self$.contrasts[[jmvcore::toB64(var)]]
                         },
                         contrasts64=function(var64) {
                           self$.contrasts[[var64]]
                         },
                         
                         nicecontrasts=function(var) {
                           a<-self$.contrasts[[jmvcore::toB64(var)]]
                           sapply(a, function(x) self$.onename(x))
                         },
                         contrastsLabels=function(var) {
                           conts<-self$.contrasts[[jmvcore::toB64(var)]]
                           self$nicelabels(conts)
                         },
                         
                         translate=function(astring) {
                           res<-astring
                           for (a in names(self$.factors)) 
                             res<-gsub(a,self$.factors[[a]],res)
                           for (a in names(self$.covs)) 
                             res<-gsub(a,self$.covs[[a]],res)
                           return(res)
                         },
                         factorize=function(vars) {
                           results<-list()
                           for (i in seq_along(vars)) {
                             term<-vars[[i]]
                             if (length(term)==1) {
                               results<-c(results,self$.factorize(term))
                             } else {
                               terms<-sapply(vars[[i]],function(term) {
                                 self$.factorize(term)        
                               })
                               int<-expand.grid(as.list(terms),stringsAsFactors = F)
                               results<-c(results,lapply(seq_len(nrow(int)), function(i) unlist(int[i,])))
                             }
                           }
                           results
                           
                         },
                         .factorize=function(term) {
                           terms<-NULL
                           if (term %in% names(self$.factors)) {
                             cont<-self$contrasts64(term)
                             for (cc in cont)
                               terms<-c(terms,cc)
                           } else
                             terms<-c(terms,term)
                           terms
                         },
                         
                         .onename=function(var64) {
                                  x<-unlist(strsplit(var64,"_._._",fixed = T))
                                  if (length(x)==2) 
                                         return(paste0(jmvcore::fromB64(x[1]),x[2]))
                                  if (var64 %in% names(self$.factors))   
                                         return(self$.factors[[var64]])
                                  if (var64 %in% names(self$.covs))   
                                         return(self$.covs[[var64]])
                                  return(var64)                         
                         },
                         .onelabel=function(obj) {
                           if (obj %in% names(self$.labels))  
                                return(self$.labels[obj])
                           if (obj %in% names(self$.covs))
                                return(self$.covs[obj])
                           return(self$.onename(obj))
                           },
                         print=function() {
                           print(self$.factors)
                           print(self$.covs)
                           print(self$.labels)
                           print(self$.contrasts)
                           
                         }
                       )

)

