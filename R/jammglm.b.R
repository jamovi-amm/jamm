### This class takes care of passing all information from lavaan tables definitions and estimations  from jamovi input
### to jamovi results tables. It wokrs using Syntax R6 class, Estimate R6 class, and Plotter R6 class.
### Syntax R6 class gets all input options and defines the tables required for showing the results. Estimate R6 class inherit from Syntax
### all properties of the tables and fill them with the actual results estimated with lavaan() function.
### Estimate inherit from Syntax, so only one instance of Estimate is defined. Here is called lav_machine
### Filling the results tables is handle by function in jamovi.R (functions starting with j.)
### Data are handled by a Datamatic R6 class, which does all transformations and checking required.


jammGLMClass <- R6::R6Class(
    "jammGLMClass",
    inherit = jammGLMBase,    private = list(
        .factors=NULL,
        .lav_machine=NULL,
        .data_machine=NULL,
        .plot_machine=NULL,
        .model=NULL,
        .ready=NULL,
        .init = function() {
            ginfo("init")
            ### check that we have enough information to run ####
            private$.ready<-readiness(self$options)
            if (!private$.ready$ready) {
                  if(private$.ready$report)
                      self$results$infotab$addRow("info",list(info="Setup",specs=private$.ready$reason))
                return()
            }
            ### prepare R6 classes that do the work ####
            data_machine<-Datamatic$new(self$options,self$data)
            lav_machine<-Estimate$new(self$options,data_machine)
            plot_machine<-Plotter$new(self$options,data_machine,lav_machine,self$results$pathgroup)
            
            ### fill the info table ###
            j.init_table(self$results$infotab,lav_machine$tab_info)
            j.init_table_append(self$results$infotab,lav_machine$models())
            j.init_table_append(self$results$infotab,lav_machine$tab_indirect)


            j.init_table(self$results$models$r2,lav_machine$tab_r2,ci=T,ciwidth=self$options$ciWidth)

            #### parameter estimates table ####
            j.init_table(self$results$models$interactions,lav_machine$tab_interactions,ci=T,ciwidth=self$options$ciWidth)
            
            #### parameter estimates table ####
            j.init_table(self$results$models$coefficients,lav_machine$tab_coefficients,ci=T,ciwidth=self$options$ciWidth)



            ### prepare regressions (Total effects / Full model / per-mediator regressions) ###
            regressions.init(self$options,self$results,lav_machine)

            
            
            
            
            
            
            # #### contrast tables ####
             if (length(self$options$factors)>0) {
                for (factor in self$options$factors) {
                 clabs<-data_machine$contrasts_labels[[factor]]
                 for (i in seq_along(clabs)) {
                       clab<-clabs[[i]]
                       self$results$models$contrastCodeTable$addRow(paste0(factor,i),list(rname=paste0(factor,i),clab=clab))
                 }
                 self$results$models$contrastCodeTable$setVisible(TRUE)    
             }
             }
            

            private$.lav_machine<-lav_machine
            private$.data_machine<-data_machine
            plot_machine$initPlots()
            private$.plot_machine<-plot_machine 
            
            
        },
    
        .run = function() {
            ginfo("run")
            ### check that we have enough information to run ####
            if (!private$.ready$ready)
                return()

            ### clean the data and prepare things ###
            lav_machine<-private$.lav_machine
            data<-private$.data_machine$cleandata(self$data,lav_machine$interactions)

            lav_machine$estimate(data)

            warns<-lav_machine$warnings
            if (is.something(warns[["main"]]))
                for (i in seq_along(warns[["main"]]))
                      self$results$infotab$setNote(i,warns[["main"]][[i]])

            if (is.something(lav_machine$errors)) {
                    stop(paste(lav_machine$errors,collapse = "; "))
            } 

            ## fit info
            j.fill_table(self$results$infotab,lav_machine$tab_info)
            
            ## interactions
            j.fill_table(self$results$models$interactions,lav_machine$tab_interactions)
            
            
            
            ### parameters estimates ####
            j.fill_table(self$results$models$coefficients,lav_machine$tab_coefficients)

            j.fill_table(self$results$models$r2,lav_machine$tab_r2)
            j.add_warnings(self$results$models$r2,lav_machine,"r2")

            ### fill regressions (Total effects / Full model / per-mediator regressions) ###
            regressions.results(self$options,data,self$results,lav_machine)




            ## diagrams
            private$.plot_machine$preparePlots()   
            if (is.something(private$.plot_machine$warnings$diagram)) {
                 for (i in seq_along(private$.plot_machine$warnings$diagram))
                        self$results$pathgroup$notes$addRow(i,list(message=private$.plot_machine$warnings$diagram[[i]]))
                  self$results$pathgroup$notes$setVisible(TRUE)
            }
            
            self$results$.setModel(lav_machine$model)
        },
 
        .showDiagram=function(image,ggtheme, theme, ...) {
            if (self$options$diagram==FALSE) 
                return()
            if (!is.something(image$state$semModel))
                 return()
            options<-private$.plot_machine$semPathsOptions
            res<-try_hard(
            semPlot::semPaths(object = image$state$semModel,
                              layout =options$layout,
                              residuals = options$residuals,
                              rotation = options$rotation,
                              intercepts = options$intercepts,
                              nodeLabels= options$nodeLabels,
                              whatLabels=options$whatLabels,
                              sizeMan = options$sizeMan,
                              sizeMan2=options$sizeMan2,
                              curve=options$curve,
                              shapeMan=options$shapeMan,
                              edge.label.cex =options$edge.label.cex)
            )
            note<-FALSE
            
            if (!isFALSE(res$error)) {
                if  (length(grep("Circle layout only supported",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("graph_from_edgelist",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("subscript out of",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["fail"]]
                    note<-TRUE
                }
            }
            
            
            
            if (!isFALSE(res$error)) {
                 self$results$pathgroup$notes$addRow("err",list(message=res$error))
                 note<-TRUE
            }
            if (!isFALSE(res$warning)) {
                self$results$pathgroup$notes$addRow("war",list(message=res$warning))
                note<-TRUE
            }

            if (note)
                self$results$pathgroup$notes$setVisible(TRUE)

            return(TRUE)

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
        
        .formula = function() {
            if (!is.something(private$.lav_machine))
                  return("")
            paste0("list(",paste(sapply(private$.lav_machine$models(),function(m) paste0('"',m$value,'"')),collapse = ","),")")
            
        },
        
        .sourcifyOption = function(option) {
            
            name <- option$name
            value <- option$value
            
            if (!is.something(value))
                return('')
            
            if (option$name %in% c('factors', 'endogenous', 'covs', 'endogenousTerms'))
                return('')
            
            if (name =='scaling') {
                vec<-sourcifyList(option,"none")
                return(vec)
            }
            if (name =='contrasts') {
                vec<-sourcifyList(option,"simple")
                return(vec)
            }
            if (name =='varcov') {
                vec<-lapply(self$options$varcov, function(v) c(v$i1,v$i2))
                vec=paste0("varcov=list(",paste(vec,collapse = ","),")",collapse = "")
                return(vec)
            }
            
            super$.sourcifyOption(option)
        }
        
        
        
        
        
        )
)
