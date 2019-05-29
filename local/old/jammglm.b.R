jammGLMClass <- R6::R6Class(
  "jammGLMClass",
  inherit = jammGLMBase,
  private=list(
    .model=NA,
    .names64=NA,
    .infos=NULL,
    .cov_condition=conditioning$new(),
    .init=function() {
      mark("init")
      private$.names64<-names64$new()
      dep<-self$options$dep
      covs<-self$options$covs
      factors<-self$options$factors
      mediators<-self$options$mediators
      ciWidth<-self$options$ciWidth
      ciType<-self$options$ciType
      
      ### here we initialize things ####
      data<-private$.cleandata()
      infos<-private$.prepareDiagram() 
      private$.infos<-infos
      
      if (infos$isImpossible)   return()
      if (infos$hasRequired())   return()
      
      meds<-lapply(infos$original_medmodels, function(m) {
        m$ind=private$.names64$factorize(m$ind)
        m
      })
      
      full<-infos$original_fullmodel
      full$ind<-private$.names64$factorize(full$ind)
      
      mods<-lapply(infos$moderators, function(m) {
        private$.names64$factorize(m)
      })
      
      infos<-smartMediation$new(meds,full,moderators = mods)
      private$.infos<-infos
      ## prepare main result table
       table<-self$results$models$main
       if (is.something(infos$moderators)) {
         modtable<-self$results$models$moderationEffects
         modtable$setVisible(TRUE)
         mr.initInteractionTable(infos,table)
         mr.initConditionalTable(infos,table,private$.names64,private$.cov_condition,ciType,ciWidth,self$options$tableOptions)
       }
       else
         mr.initTable(infos,table,private$.names64,ciType,ciWidth,self$options$tableOptions)
       
      if  (is.something(self$options$factors))   
          mi.initContrastCode(data,self$options,self$results,private$.names64)
      
    },
    .run=function() {
      n64<-private$.names64
      mark("run")
      # collect some option
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      mediators <- self$options$mediators
      infos<-private$.infos
      ciWidth<-self$options$ciWidth/100
      ciType<-self$options$ciType
      bootN<-self$options$bootN
      
      if (is.null(dep))
        return()
      if (is.null(mediators))
        return()
      if (is.null(covs) && is.null(factors))
        return()
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
      ###############      
      data<-private$.cleandata()
      for (scaling in self$options$scaling) {
        data[[jmvcore::toB64(scaling$var)]]<-lf.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type)  
      }

      if (!is.null(covs)) {
        private$.cov_condition$storeValues(data)
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      ## fill main mediational results
      ## notice that jmf.modelSummaries return first the individual coefficients
      ## and then the mediated effect. Because in .init the mediated effec is defined ad
      ## the first row, it formats the table well because it uses the rowKey appropriately
      if (!infos$isEstimable())
         return()
      se<-ifelse(ciType=="standard" || ciType=="none",ciType,"bootstrap")
      params<-jmf.mediationTable(infos,data,level = ciWidth,se=se, boot.ci=ciType,bootN=bootN)
      table<-self$results$models$main
      table$setNote("cinote",paste("(a) Confidence intervals computed with method:",NOTES[["ci"]][[ciType]]))
      table$setVisible(TRUE)
      
      if (!is.something(infos$moderators)) {
           for (rowKey in table$rowKeys) {
               row<-params[params$label==rowKey,]
               if (dim(row)[1]>0)
                  table$setRow(rowKey=rowKey,row)
           }
      } else {
        # first we fill the interaction table    
        modtable<-self$results$models$moderationEffects

        moderators<-unique(unlist(sapply(infos$moderators,n64$factorName)))
        moderators64<-jmvcore::toB64(moderators)
        
        itable<-params[(params$op=="~" & params$model=="med"),]
        where<-grep("____",itable$rhs, fixed=T)
        itable<-itable[where,]
        inters<-list()
        for (i in 1:nrow(itable)) {
          row<-itable[i,]
          w<-strsplit(row$rhs,"____")[[1]]
          nicew<-n64$nicenames(w)
          row$mod<-paste(nicew[w %in% infos$moderators],collapse = ":")
          target<-jmvcore::composeTerm(nicew)
          row$target<-.nicifychain(c(target,jmvcore::fromB64(row$lhs)))
          row$rowKey<-paste(row$lhs,row$rhs,sep="_")
          inters[[row$rowKey]]<-row
        }
        ointers<-do.call(rbind,inters)
        ointers<-ointers[order(ointers$mod),]
        for (i in seq_len(nrow(ointers))) 
            modtable$addRow(rowKey=ointers[i,"rowKey"],ointers[i,]) 
        
         # now we fill the simple medation table
        ncombs<-expand.levels_numeric(moderators64,private$.cov_condition)
        mnames<-names(ncombs)
        lcombs<-expand.levels(moderators64,private$.cov_condition)
        mark(lcombs)
        for (j in 1:nrow(ncombs)) {     
          ldata<-data
          for (mname in mnames) {
            condata<-private$.cov_condition$center(mname,ldata,ncombs[j,mname])
            for (var in names(condata)) {
              ldata[,var]<-condata[,var]
            }
          }
           tableKeys<-table$rowKeys
           params<-jmf.mediationTable(infos,ldata,level = ciWidth,se=se, boot.ci=ciType,bootN=bootN)
           for (i in seq_along(params$label)) {
              row<-params[i,]
              for (name in names(lcombs))
                row[[name]]<-lcombs[j,name]
              rowKey<-paste(j,row$label,sep="_..._")
              if (rowKey %in% tableKeys) {
                 table$setRow(rowKey=rowKey,row)
              }
        }
        }
      }
    },
  .cleandata=function() {
      n64<-private$.names64
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      mediators<-self$options$mediators

      dataRaw <- self$data
      data <- list()
      
      if ( ! is.null(dep)) {
        data[[jmvcore::toB64(dep)]] <- jmvcore::toNumeric(dataRaw[[dep]])
        n64$addVar(dep)
      }
      
      for (covariate in covs) {
        data[[jmvcore::toB64(covariate)]] <- jmvcore::toNumeric(dataRaw[[covariate]])
        n64$addVar(covariate)
      }
      ### initialize conditioning of covariates
      if (!is.null(self$options$covs)) {
        span<-ifelse(self$options$simpleScale=="mean_sd",self$options$cvalue,self$options$percvalue)
        vars64<-jmvcore::toB64(self$options$covs)
        private$.cov_condition<-conditioning$new(vars64,self$options$simpleScale,span)
      }
      #####################
      for (med in mediators) {
        data[[jmvcore::toB64(med)]] <- jmvcore::toNumeric(dataRaw[[med]])
        n64$addVar(med)
      }

      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          info(paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
        factor64<-jmvcore::toB64(factor)
        data[[factor64]] <- dataRaw[[factor]]
        levels <- base::levels(data[[factor64]])
        stats::contrasts(data[[factor64]]) <- lf.createContrasts(levels,"deviation")
        n64$addFactor(factor,levels)
        n64$addLabel(factor,lf.contrastLabels(levels, "deviation")) 
        attr(data[[factor64]],"jcontrast")<-"deviation"
        private$.cov_condition$addFactor(factor64,levels)
      }
      
      for (contrast in self$options$contrasts) {
        var64<-jmvcore::toB64(contrast$var)
        levels <- base::levels(data[[var64]])
        stats::contrasts(data[[var64]]) <- lf.createContrasts(levels, contrast$type)
        n64$addLabel(contrast$var,lf.contrastLabels(levels, contrast$type)) 
        attr(data[[var64]],"jcontrast")<-contrast$type
        dummies<-model.matrix(as.formula(paste0("~",jmvcore::toB64(contrast$var))),data=data)
        dummies<-dummies[,-1]
        dummies<-data.frame(dummies)
        names(dummies)<-unlist(n64$contrasts(contrast$var))
        data<-cbind(data,dummies)
      }
      
      private$.names64<-n64
      data<-as.data.frame(data)      

      data <- jmvcore::naOmit(data)
      return(data)
      
    },


.prepareDiagram=function() {

  infoTable<-self$results$info
  
  n64<-private$.names64
  
  dep64<-jmvcore::toB64(self$options$dep)
  covs64<-jmvcore::toB64(self$options$covs)
  factors64<-jmvcore::toB64(self$options$factors)
  mediators64<-jmvcore::toB64(self$options$mediators)
  modelTerms<-self$options$modelTerms

  mediatorsTerms<-self$options$mediatorsTerms
  moderatorsTerms<-self$options$moderatorsTerms

  n64<-private$.names64
  
  ### update model info table
  goon<-ds.initModelInfo(self)  
  
  ## build the models list
  medmodels64<-list()
  for (i in seq_along(mediators64))  
       medmodels64[[i]]<-list(dep=mediators64[i],ind=sapply(mediatorsTerms[[i]],jmvcore::toB64))

  fullmodel64<-list(dep=dep64,ind=sapply(modelTerms,jmvcore::toB64))

  modTerms64<-moderatorsTerms
    for (i in seq_along(mediators64))  
       for (j in seq_along(moderatorsTerms[[i]]))
         modTerms64[[i]][[j]]<-jmvcore::toB64(moderatorsTerms[[i]][[j]])
       

  #### let smart do the magic ####
  infos<-smartMediation$new(medmodels64,fullmodel64,moderators = modTerms64)
  #### prepare the diagram
  image <- self$results$pathmodelgroup$get('pathmodel')
  paths<-diag.paths(infos,suggested = T,shiftmed=.01)

  # for (i in seq_along(paths$labs))
  #         if (paths$labs[[i]] %in% factors) {
  #            n<-length(n64$nicecontrasts(paths$labs[[i]]))
  #            paths$labs[[i]]<-paste0(paths$labs[[i]]," (",paste(1:n,collapse=","),")")
  #         }
  ## save the results for showing later      
  image$setState(list(paths=paths,infos=infos))
  #### includes possible diagrams notes 
      notes<-self$results$pathmodelgroup$pathnotes
      ds.annotate.diagram(infos,paths,notes,self$options,n64)       
   ds.modelInfo(infos,self,n64)
   return(infos)      
      
},  

.showDiagram=function(image, ggtheme, theme, ...) {

    if (is.null(image$state))
        return()
  
  
  infos<-image$state$infos
  paths<-image$state$paths
  box.size=.1+(max(length(infos$mediators),length(infos$independents))+3)^-8
  box.text=.80+(infos$nvars)^-2
  arr.lenght=1/(infos$nvars-1)
  labs<-jmvcore::fromB64(paths$labs)
  labs<-gsub(">","*",labs,fixed=T)  
  ### first we plot the linear models paths diagram
  plot<-diagram::plotmat(paths$paths, pos=paths$pos, 
                  name= labs,box.size = box.size,
                  box.type = "rect", box.prop=.4, box.cex = box.text , curve=paths$curves,
                  endhead=F, arr.type="triangle",arr.length = arr.lenght,
                  ,arr.pos=.7,arr.col = "gray",lcol = paths$colors,box.lcol=paths$bcolors)

  test<-try({
  ### then we add the moderators
  mp<-infos$moderatedPaths()
   for (i in seq_along(mp)) 
     for (j in seq_along(mp[[i]])) {
          coord=mp[[i]][[j]]
          diag.plot_mod_arr(plot,coord$from,coord$to,i)
        }
  })
  if (jmvcore::isError(test)) {
    self$results$pathmodelgroup$pathmodel$setVisible(FALSE)
    self$results$pathmodelgroup$pathnotes$addRow(rowKey="noluck",list(info=NOTES$diag$noluck))
    return(TRUE)
  }
    
  diag.plot_mods(plot,infos$moderators)
  TRUE
},
.sourcifyOption = function(option) {
        name <- option$name
        value <- option$value
        
        super$.sourcifyOption(option)
}
))


