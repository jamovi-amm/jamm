Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
    factors=NULL,
    factors_scale=NULL,
    factors_levels=list(),
    contrasts_labels=NULL,
    contrasts_values=NULL,
    contrasts_names=NULL,
    continuous=NULL,
    continuous_scale=NULL,
    initialize=function(options,data) {
      super$initialize(options=options,vars=unlist(c(options$dep,options$mediators,options$factors,options$covs)))
      self$factors<-options$factors
      self$factors_scale<-sapply(options$contrasts, function(a) a$type)
      names(self$factors_scale)<-sapply(options$contrasts, function(a) a$var)
      self$continuous<-unlist(c(options$mediators,options$covs))
      self$continuous_scale<-sapply(options$scaling, function(a) a$type)
      names(self$continuous_scale)<-sapply(options$scaling, function(a) a$var)
      private$.inspect_data(data)
    },
    cleandata=function(data,interactions=NULL) {
      
      data64 <- jmvcore::naOmit(data)
      names(data64)<-tob64(names(data))
      for (cont in self$continuous) {
        cont64<-tob64(cont)
        if (class(data64[[cont64]]) == "factor")
              self$warnings<-list(topic="main",message=DATA_WARNS[["fac_to_cont"]])
        data64[[cont64]] <- private$.continuous_value(data64[[cont64]],self$continuous_scale[[cont]])
      }
      for (factor in self$factors) {
        factor64<-tob64(factor)
        ### we need this for Rinterface ####
        nlevels<-length(self$factors_levels[[factor]])
        stats::contrasts(data64[[factor64]]) <- self$contrasts_values[[factor]]
        dummies<-model.matrix(as.formula(paste0("~",factor64)),data=data64)
        dummies<-dummies[,-1]
        dummies<-data.frame(dummies)
        onames<-names(data64)
        data64<-cbind(data64,dummies)
        fn64<-paste0(factor64,FACTOR_SYMBOL,1:(nlevels-1))
        fn<-paste0(factor,1:(nlevels-1))
        names(data64)<-c(onames,fn64)
      }
        for (int in interactions) {
          int<-trimws(int)
          terms<-strsplit(int,INTERACTION_SYMBOL,fixed = T)[[1]]
          terms<-paste0("data64$",trimws(terms))
          head<-paste0("data64$",int,"<-")
          op<-paste(terms,collapse = " * ")
          synt<-paste0(head,op)
          eval(parse(text=synt))
        }
        
      data64<-as.data.frame(data64)     
      return(data64)
      
      
      
        
    }
    ), ### end of public
   private=list(
     .inspect_data=function(data) {
            names(data)<-jmvcore::toB64(names(data))
            for (factor in self$factors) {
                    self$factors_levels[[factor]]<-levels(data[,tob64(factor)])
                    for (i in seq_along(levels(data[,tob64(factor)])))
                          self$contrasts_names[[paste0(factor,i)]]<-paste0(tob64(factor),FACTOR_SYMBOL,i)
            }
     
            private$.create_constrasts()

       },
     .create_constrasts=function() {
       
       self$contrasts_labels<-sapply(self$factors,function(factor) 
                         private$.contrast_label(self$factors_levels[[factor]],self$factors_scale[[factor]]),simplify = FALSE)  
       
       self$contrasts_values<-sapply(self$factors,function(factor) 
                         private$.contrast_value(self$factors_levels[[factor]],self$factors_scale[[factor]]),simplify = FALSE)  
     },
     .contrast_value=function(levels, type) {
       
       nLevels <- length(levels)
       
       if (is.null(type))
         type<-"simple"
       

       if (type == 'simple') {
         dummy <- stats::contr.treatment(levels)
         dimnames(dummy) <- NULL
         coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
         contrast <- (dummy - coding)
         
       } else if (type == 'deviation') {
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[i+1, i] <- 1
           contrast[1, i] <- -1
         }
         
       } else if (type == 'difference') {
         
         contrast <- stats::contr.helmert(levels)
         for (i in 1:ncol(contrast))
           contrast[,i] <- contrast[,i] / (i + 1)
         
         dimnames(contrast) <- NULL
         
       } else if (type == 'helmert') {
         
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         
         for (i in seq_len(nLevels-1)) {
           p <- (1 / (nLevels - i + 1))
           contrast[i,i] <- p * (nLevels - i)
           contrast[(i+1):nLevels,i] <- -p
         }
         
       } else if (type == 'polynomial') {
         
         contrast <- stats::contr.poly(levels)
         dimnames(contrast) <- NULL
         
       } else if (type == 'repeated') {
         
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[1:i,i] <- (nLevels-i) / nLevels
           contrast[(i+1):nLevels,i] <- -i / nLevels
         }
         
       } else if (type == 'dummy') {
         contrast <- stats::contr.treatment(levels,base=1)
         dimnames(contrast) <- NULL
       } else {
         contrast <- matrix(0, nrow=nLevels, ncol=nLevels-1)
         for (i in seq_len(nLevels-1)) {
           contrast[i+1, i] <- 1
           contrast[1, i] <- -1
         }
       }
       dimnames(contrast)<-list(NULL,1:(nLevels-1))
       contrast
     },
     .contrast_label=function(levels, type) {

         nLevels <- length(levels)
         labels <- list()
         
         if (is.null(type))
             type<-"simple"
         
         if (type == 'simple') {
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '-', levels[1])
           return(labels)
         } 
         
         if (type == 'dummy') {
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '-', levels[1])
           return(labels)
         } 
         
         if (type == 'deviation') {
           all <- paste(levels, collapse=', ')
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i+1], '- (', all,")")
           return(labels)
           
         } 
         
         if (type == 'difference') {
           
           for (i in seq_len(nLevels-1)) {
             rhs <- paste0(levels[1:i], collapse=', ')
             if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
             labels[[i]] <- paste(levels[i + 1], '-', rhs)
           }
           return(labels)
         }
         
         if (type == 'helmert') {
           
           for (i in seq_len(nLevels-1)) {
             rhs <- paste(levels[(i+1):nLevels], collapse=', ')
             if (nchar(rhs)>1) rhs<-paste0(" (",rhs,")")
             labels[[i]] <- paste(levels[i], '-', rhs)
           }
           return(labels)
         }
         
         
         
         if (type == 'repeated') {
           
           for (i in seq_len(nLevels-1))
             labels[[i]] <- paste(levels[i], '-', levels[i+1])
           return(labels)
           
         } 
         if (type == 'polynomial') {
           names <- c('linear', 'quadratic', 'cubic', 'quartic', 'quintic', 'sextic', 'septic', 'octic')
           for (i in seq_len(nLevels-1)) {
             if (i <= length(names)) {
               labels[[i]] <- names[i]
             } else {
               labels[[i]] <- paste('degree', i, 'polynomial')
             }
           }
           return(labels)
         }
         mark("no contrast definition met")
         
         all <- paste(levels, collapse=', ')
         for (i in seq_len(nLevels-1))
           labels[[i]] <- paste(levels[i+1], '- (', all,")")
         return(labels)
       },

 .continuous_value=function(var,method,by=NULL) {

        if (is.null(method))
           return(as.numeric(var))
   

       if (method=="centered") 
         var<-scale(var,scale = F)  
       if (method=="cluster-based centered") {    
         var<-unlist(tapply(var,by,scale,scale=F))
       }
       if (method=="standardized") 
         var<-scale(var,scale = T)  
       if (method=="cluster-based standardized")     
         var<-unlist(tapply(var,by,scale,scale=T))

       as.numeric(var)
       
     }
     
     
     
   ) #end of private
)