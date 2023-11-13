jammGLMClass <- R6::R6Class(
  "jammGLMClass",
  inherit = jammGLMBase,
  private=list(
    .model=NA,
    .names64=NA,
    .infos=NULL,
    .paths=NULL,
    .infos64=NULL,
    .clean_data=NULL,
    .fit=NULL,
    .cov_condition=conditioning$new(),
    .init=function() {
      jinfo("init")
      Sys.getlocale("LC_NUMERIC")
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
 
      meds<-lapply(infos$original_medmodels, function(m) {
        m$ind=private$.names64$factorize(m$ind)
        m
      })
      
      full<-infos$original_fullmodel
      full$ind<-private$.names64$factorize(full$ind)
      
      mods<-lapply(infos$moderators, function(m) {
        private$.names64$factorize(m)
      })
      
      infos64<-smartMediation$new(meds,full,moderators = mods)
      private$.infos64<-infos64
      
      if (infos$isImpossible)   return()
      if (infos$isEmpty)        return()
#      if (infos$hasRequired())  return()
      ## prepare main result table
      
       table<-self$results$models$main
       
       if (is.something(infos64$moderators)) {
         modtable<-self$results$models$moderationEffects
         modtable$setVisible(TRUE)
         mr.initConditionalTable(infos64,table,private$.names64,private$.cov_condition,ciType,ciWidth,self$options$tableOptions)
       }
       else
         mr.initTable(infos64,table,private$.names64,ciType,ciWidth,self$options$tableOptions)
       
      if  (is.something(self$options$factors))   
          mr.initContrastCode(data,self$options,self$results,private$.names64)

       if ("regression" %in% self$options$tableOptions) 
          regressions.init(infos64,data,self$options,self$results,private$.names64)

    },
    .run=function() {
      n64<-private$.names64
      jinfo("run")
      # collect some option
      dep <- self$options$dep
      if (is.null(dep))
        return()
      
      factors <- self$options$factors
      covs <- self$options$covs
      mediators <- self$options$mediators
      infos64<-private$.infos64
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
      private$.clean_data<-data

      for (scaling in self$options$scaling) {
        data[[jmvcore::toB64(scaling$var)]]<-lf.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type)  
      }

      if (!is.null(covs)) {
        private$.cov_condition$storeValues(data)
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      ## fill main mediational results
      ## notice that jmf.modelSummaries return first the individual coefficients
      ## and then the mediated effect. Because in .init the mediated effect is defined at
      ## the first row, this function fills the table well because it uses the rowKey appropriately
     
#      if (!infos64$isEstimable())
#         return()
      mark("first estimate of the model")
      se<-ifelse(ciType=="standard" || ciType=="none",ciType,"bootstrap")


      
      ## fill the main tables
      params<-jmf.mediationTable(infos64,
                                 data,
                                 level = ciWidth,
                                 se=se, 
                                 boot.ci=ciType,
                                 bootN=bootN,
                                 missing=self$options$missing)
      private$.fit<-attr(params,"fit")
      
      ## update info table ####
      
      self$results$info$addRow(rowKey="blank",list(info="",specs="",value=""))
      self$results$info$addRow(rowKey="n",
                               list(info="Sample size",specs="N",
                                    value=lavaan::lavInspect(private$.fit,"nobs"))
                               )
     
      #####
      table<-self$results$models$main
      if (ciType!="none")
          table$setNote("cinote",paste("Confidence intervals computed with method:",NOTES[["ci"]][[ciType]]))
          table$setNote("betas",paste("Betas are completely standardized effect sizes"))
      
#      table$setVisible(TRUE)

      if (!is.something(infos64$moderators)) {
           for (rowKey in table$rowKeys) {
               row<-params[params$label==rowKey,]
               if (dim(row)[1]>0)
                  table$setRow(rowKey=rowKey,row)
           }
      } else {
        # first we fill the interaction table    
        modtable<-self$results$models$moderationEffects

        moderators<-unique(unlist(sapply(infos64$moderators,n64$factorName)))
        moderators64<-jmvcore::toB64(moderators)
        
        itable<-params[(params$op=="~" & params$model=="med"),]
        where<-grep("____",itable$rhs, fixed=T)
        if (length(where)==0)
             jmvcore::reject("A moderator is specified by no interaction is present in the  models")
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
        for (j in 1:nrow(ncombs)) {     
          ldata<-data
          for (mname in mnames) {
            condata<-private$.cov_condition$center(mname,ldata,ncombs[j,mname])
            for (var in names(condata)) {
              ldata[,var]<-condata[,var]
            }
          }
           tableKeys<-table$rowKeys
           params<-jmf.mediationTable(infos64,
                                      ldata,
                                      level = ciWidth,
                                      se=se, 
                                      boot.ci=ciType,
                                      bootN=bootN,
                                      missing=self$options$missing)
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
      mark(" estimate end")
      
      if ("regression" %in% self$options$tableOptions) 
        regressions.results(infos64,data,self$options,self$results,private$.names64)
          
      out.table_notes(self$results$info,attr(data,"warning"))
    },
  .cleandata=function() {
      n64<-private$.names64
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      mediators<-self$options$mediators
      .warning<-list()
      ## now (1.2.0) we allow missing values
#      dataRaw <- jmvcore::naOmit(self$data)
      dataRaw<-self$data
      data <- list()
      if ( ! is.null(dep)) {
        if (class(dataRaw[[dep]]) == "factor")
          .warning<-append(.warning,"Warming: The dependent variable is defined as factor. Please make sure it is a continuous variable.")
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
      .contrasts<-sapply(self$options$contrasts,function(a) a$type)
      .contrastsnames<-sapply(self$options$contrasts,function(a) a$var)
      names(.contrasts)<-.contrastsnames
      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          jinfo(paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
        factor64<-jmvcore::toB64(factor)
        data[[factor64]] <- dataRaw[[factor]]
        levels <- base::levels(data[[factor64]])
        .cont<-ifelse(factor %in% .contrastsnames,.contrasts[[factor]],"simple")
        stats::contrasts(data[[factor64]]) <- lf.createContrasts(levels,.cont)
        n64$addFactor(factor,levels)
        n64$addLabel(factor,lf.contrastLabels(levels, .cont)) 
        attr(data[[factor64]],"jcontrast")<-.cont
        private$.cov_condition$addFactor(factor64,levels)
        dummies<-model.matrix(as.formula(paste0("~",factor64)),data=data)
        dummies<-dummies[,-1]
        dummies<-data.frame(dummies)
        names(dummies)<-unlist(n64$contrasts(factor))
        data<-cbind(data,dummies)
        
      }

      private$.names64<-n64
      data<-as.data.frame(data)     
      attr(data,"warning")<-.warning
      return(data)
      
    },


.prepareDiagram=function() {

  infoTable<-self$results$info
  
  n64<-private$.names64
  suggested<-("suggested" %in% self$options$pathOptions)
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
  paths<-diag.paths(infos,suggested = suggested,shiftmed=.01)

  # for (i in seq_along(paths$labs))
  #         if (paths$labs[[i]] %in% factors) {
  #            n<-length(n64$nicecontrasts(paths$labs[[i]]))
  #            paths$labs[[i]]<-paste0(paths$labs[[i]]," (",paste(1:n,collapse=","),")")
  #         }
  ## save the results for showing later      
# image$setState(list(paths=paths,infos=infos))
  
  private$.infos<-infos
  private$.paths<-paths
  #### includes possible diagrams notes 
  notes<-self$results$pathmodelgroup$pathnotes
  ds.annotate.diagram(infos,paths,notes,self$options,n64)       
  ds.modelInfo(infos,self,n64)
  return(infos)      
      
},  

.showStatDiagram=function(image, ggtheme, theme, ...) {
  
  if (is.null(private$.infos))
    return()
  if (is.null(self$options$diagram != "statistical"))
    return()
  if (private$.infos$isEmpty | private$.infos$isImpossible) 
    return()
  if (is.null(private$.fit)) 
    return()
  
  fit<-private$.fit
  ie<-private$.infos$ieffects
  pt<-fit@ParTable
  lhs<-pt$lhs[pt$op!=":="]
  xcoo<-sapply(unique(lhs), function(x) {
    max((unlist(lapply(ie, function(xx) which(xx==x)))))
  })
  xcoo[!is.finite(xcoo)]<-1
  
  q<-cbind(seq_along(xcoo),order(xcoo))
  orig_order<-q[order(q[,2]),1]
  xcoo<-xcoo[order(xcoo)]
  if (length(unique(xcoo))==length(xcoo)) {
          ycoo<-rep(.80,length(xcoo))
          ycoo[xcoo==min(xcoo)]<-ycoo[xcoo==max(xcoo)]<-.2
  } else {
          ycoo<-unlist(lapply(unique(xcoo), function(x) {
          nvars<-length(xcoo[xcoo==x])
          1:nvars/(nvars+1)
          }))
  }
  p<-cbind(x=xcoo,y=ycoo)
  p<-p[orig_order,]

  ## take care of the labels  and nodes size
  labs<-fit@pta$vnames$ov.num[[1]]
  nNodes<-length(labs)
  
  size<-12
  if (self$options$diag_labsize=="small") size<-8
  if (self$options$diag_labsize=="large") size<-18
  if (self$options$diag_labsize=="vlarge") size<-24
  size<-size*exp(-nNodes/80)+1
  labs<-fromb64(labs)
  
  if (self$options$diag_abbrev!="0")
    labs<-abbreviate(labs,minlength = as.numeric(self$options$diag_abbrev),strict = T)
  labs<-gsub("`","",labs)
  
  ## make the plot
  sp<-semPlot::semPaths(fit, 
               sizeMan=size,
               sizeMan2=size/2,
               edge.label.cex = 1,
               label.cex=.9,
               style = "ram",
               residuals = F,
               nCharNodes = 0, nCharEdges = 0,
               layout = p,
               exoVar=T,
               nodeLabels = labs,
               shapeMan=self$options$diag_shape,
               whatLabels=self$options$diag_paths)

  ## adjust label position and curvature 
  
  if (self$options$diag_offset)
        sp$graphAttributes$Edges$edge.label.position<-rep(.60,length(sp$graphAttributes$Edges$edge.label.position))
  
  sp$graphAttributes$Edges$lty[sp$Edgelist$bidirectional]<-2
  sp$graphAttributes$Edges$curve[sp$Edgelist$bidirectional]<-.6
  sp$graphAttributes$Edges
  plot(sp)
  
},
.showConceptualDiagram=function(image, ggtheme, theme, ...) {

    if (is.null(private$.infos))
        return()
    if (is.null(self$options$diagram != "conceptual"))
        return()
  
  infos<-private$.infos
  paths<-private$.paths

  size<-12
  if (self$options$diag_labsize=="small") size<-8
  if (self$options$diag_labsize=="large") size<-18
  if (self$options$diag_labsize=="vlarge") size<-24
  box.size<-(size*exp(-infos$nvars/80)+1)/160
  box.text=20*box.size
  arr.lenght=1/(infos$nvars-1)
  q<-sapply(jmvcore::decomposeTerms(paths$labs), jmvcore::fromB64)
  labs<-sapply(q, jmvcore::composeTerm)
  labs<-gsub(">","*",labs,fixed=T)  
  box.text=19*box.size
  llabs<-sapply(labs, function(x) if (nchar(x)>9) box.text*(9/nchar(x)) else box.text) 

  if (self$options$diag_abbrev!="0")
    labs<-abbreviate(labs,minlength = as.numeric(self$options$diag_abbrev),strict = T)
  labs<-gsub("`","",labs)
  ### first we plot the linear models paths diagram
  plot<-diagram::plotmat(paths$paths, pos=paths$pos, 
                  name= labs,box.size = box.size,
                  box.type = "rect", box.prop=.4, box.cex = llabs , curve=paths$curves,
                  endhead=F, arr.type="triangle",arr.length = arr.lenght,
                  arr.pos=.7,arr.col = "gray",lcol = paths$colors,box.lcol=paths$bcolors)

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
    
  diag.plot_mods(plot,infos$moderators,box.size,self$options$diag_abbrev)
  TRUE
},
.marshalFormula= function(formula, data, name) {

    
    formula<-lapply(formula,expand.formula)
    
    if (name=="simpleScale")
     return("mean_sd")
  deps<-sapply(formula, function(f) jmvcore::marshalFormula(f,data,from = "lhs"))
  ivs<-unique(unlist(sapply(formula, function(f) jmvcore::marshalFormula(f,data,from = "rhs"))))
  factors<-unique(unlist(sapply(formula, function(f) jmvcore::marshalFormula(f,data,from = "rhs",permitted = "factor"))))
  covs<-setdiff(ivs,factors)
  dep<-setdiff(deps,ivs)
  meds<-intersect(deps,ivs)
  
  if (name=="dep") {
          if (length(dep)>1)
              jmvcore::reject("The models in formula imply more than one dependent variable. Please refine your models")
           return(dep)  
  }
  if (name=="factors") {
    return(factors)  
  }
  if (name=="covs") {
    return(covs)  
  }
  
  if (name=="mediators") {
    if (length(meds)==0)
      jmvcore::reject("The models in formula imply no mediator. Please refine your models")
    return(meds)  
  }
  if (name=="mediatorsTerms") {
    medsterms<-lapply(formula, function(f) {
       .dep<-jmvcore::marshalFormula(f,data,from = "lhs")
       res<-NULL
       if (.dep %in% meds)
          res<-jmvcore::marshalFormula(f,data,from = "rhs",type = "terms")
       res
    })
    medsterms<-medsterms[-which(sapply(medsterms, is.null))]
    return(medsterms)  
  }

    if (name=="modelTerms") {
    for (f in formula) {
      .dep<-jmvcore::marshalFormula(f,data,from = "lhs")
      .vars<-jmvcore::marshalFormula(f,data,from = "rhs")
      .ivs<-unique(unlist(.vars))
      if (.dep==dep & length(intersect(meds,.ivs))>0)
         return(jmvcore::marshalFormula(f,data,from = "rhs",type = "terms"))
    }
    }
  
},
.formula=function(){
  
  if (is.null(self$options$dep)) return()
  
  if (private$.infos$isEstimable()) {
    forms=private$.infos$medFormulas()
    forms[[length(forms)+1]]<-private$.infos$fullFormula()
    for (i in seq_along(forms))
      forms[[i]]=private$.names64$translate(forms[[i]])
    return(paste('list(',paste(forms,collapse = ",\n\t"),')'))      
  } else {
    return('list()')
  }
  
  
},
.sourcifyOption = function(option) {

        name <- option$name
        value <- option$value
        if (name %in% c('mediators', 'factors', 'dep', 'covs', 'cluster', 'modelTerms','mediatorsTerms'))
          return('')
        if (length(value) == 0)
            return('')
        if (name =='scaling') {
          vec<-sourcifyList(option,"centered")
          return(vec)
        }
        if (name =='contrasts') {
          vec<-sourcifyList(option,"simple")
          return(vec)
        }
        if (name =='moderatorsTerms') {
          alist<-lapply(value, unlist)
          if (all(sapply(alist,is.null)))
               return('')
          names(alist)<-jmvcore::fromB64(private$.infos$mediators)
          alist<-alist[!sapply(alist,is.null)]
          res<-paste(sapply(names(alist),function(name) {
            a<-alist[[name]]
            if (length(a)==1) 
              paste0(name,"=\"",a,sep ="\"")
            else {
              paste0(name,"=c(",paste0("\"",a,sep ="\"",collapse=","),")")
            }
          }),collapse=",")
          res<-paste0("list(",res,")")
          return(paste("moderatorsTerms",res,sep = " = "))
        }

        super$.sourcifyOption(option)
}
))


