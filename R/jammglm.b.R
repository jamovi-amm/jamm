jammGLMClass <- R6::R6Class(
    "jammGLMClass",
    inherit = jammGLMBase,
    private = list(
        .data_machine = NULL,
        .runner_machine = NULL,
        .plotter_machine = NULL,
        .ready = NULL,
        .time = NULL,
        .smartObjs = list(),
        .init = function() {
            jinfo("MODULE: jAMM  #### phase init  ####")
            private$.time <- Sys.time()

            # LEGACY (jAMM < 2.0): copy the jAMM 1.x option `moderatorsTerms` into
            # `mod_terms` before anything reads the options. Remove with R/legacy.R.
            migrate_legacy_options(self$options)

            private$.ready <- readiness(self$options)
            if (!private$.ready$ready) {
                if (private$.ready$report) {
                    self$results$infotab$addRow("info", list(info = "Setup", value = private$.ready$reason, desc = ""))
                }
                return()
            }

            # Set up the R6 workhorse classes, retaining jAMM's data API.
            data_machine <- Datamatic$new(self)
            runner_machine <- Runner$new(self, data_machine)
            private$.smartObjs <- list()

            # Information table.
            aSmartObj <- SmartTable$new(self$results$infotab, runner_machine)
            ladd(private$.smartObjs) <- aSmartObj

            # R-squared table.
            aSmartObj <- SmartTable$new(self$results$models$r2, runner_machine)
            aSmartObj$ci("", self$options$ciWidth)
            ladd(private$.smartObjs) <- aSmartObj

            # Interactions start hidden and are activated when table data exist.
            aSmartObj <- SmartTable$new(self$results$models$interactions, runner_machine)
            aSmartObj$activateOnData <- TRUE
            aSmartObj$spaceBy<- "lhs"
            aSmartObj$ci("", self$options$ciWidth)
            ladd(private$.smartObjs) <- aSmartObj

            # Indirect and total effects.
            self$results$models$coefficients$getColumn("condition")$setVisible(length(unlist(self$options$mod_terms)) > 0)
            aSmartObj <- SmartTable$new(self$results$models$coefficients, runner_machine)
            aSmartObj$ci("", self$options$ciWidth)
            aSmartObj$spaceBy <- "condition"
            ladd(private$.smartObjs) <- aSmartObj

            # Contrast definitions start hidden when no factors are specified.
            aSmartObj <- SmartTable$new(self$results$models$contrastCodeTable, runner_machine)
            aSmartObj$activateOnData <- TRUE
            ladd(private$.smartObjs) <- aSmartObj

            # Expand the optional coefficient matrices through SmartTabs, as in GAMLj.
            aSmartObj <- SmartArray$new(self$results$models$contrastCodeTables, runner_machine)
            aSmartObj$expandOnInit <- TRUE
            ladd(private$.smartObjs) <- aSmartObj

            if ("regression" %in% self$options$tableOptions) {
                self$results$regressions$setTitle("Regression Results")
                if (length(self$options$mediators) > 1) {
                    self$results$regressions$mediator_regressions$setTitle("Mediator Models")
                }

                # Total-effects ANOVA and regression tables.
                aSmartObj <- SmartTable$new(self$results$regressions$overall$anova, runner_machine)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$regressions$overall$regression, runner_machine)
                aSmartObj$title <- paste("Total effects predicting:", self$options$dep)
                aSmartObj$ci("", self$options$ciWidth)
                ladd(private$.smartObjs) <- aSmartObj

                # Each mediator owns a group containing ANOVA and coefficients.
                aSmartObj <- SmartArray$new(self$results$regressions$mediator_regressions, runner_machine)
                aSmartObj$ci("", self$options$ciWidth)
                ladd(private$.smartObjs) <- aSmartObj
                mediatorsSmartObj <- aSmartObj

                # Full-model ANOVA and regression tables.
                aSmartObj <- SmartTable$new(self$results$regressions$full$anova, runner_machine)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$regressions$full$regression, runner_machine)
                aSmartObj$title <- paste("Full model predicting:", self$options$dep)
                aSmartObj$ci("", self$options$ciWidth)
                ladd(private$.smartObjs) <- aSmartObj
            }

            # Initialize all registered wrappers, as in GAMLj.
            private$.smartObjs <- init_smart_tables(private$.smartObjs)
            if ("regression" %in% self$options$tableOptions) {
                for (index in seq_along(mediatorsSmartObj$childrenObjs)) {
                    mediatorsSmartObj$childrenObjs[[index]]$title <- paste("Dependent variable:", self$options$mediators[[index]])
                }
            }

            private$.data_machine <- data_machine
            private$.runner_machine <- runner_machine

            # Keep the existing jAMM diagram pipeline separate from table dispatch.
            plotter_machine <- Plotter$new(self, data_machine, runner_machine$estimator, self$results$pathgroup)
            plotter_machine$initPlots()
            private$.plotter_machine <- plotter_machine

            jinfo("INIT TIME:", Sys.time() - private$.time, " secs")
        },
        .run = function() {
            runnow <- Sys.time()
            jinfo("MODULE: jAMM  #### phase run ####")

            if (!private$.ready$ready) {
                return()
            }

            data <- private$.data_machine$cleandata(self$data, private$.runner_machine$estimator$interactions)
            private$.runner_machine$estimate(data)
            # Failed fits are reported in Issues, without running dependent output.
            if (!private$.runner_machine$ok)
                return()

            # Run tables through SmartTabs, never by calling the providers here.
            for (smarttab in private$.smartObjs) {
                smarttab$runTable()
            }

            # Preserve the existing diagram preparation and warning reporting.
            private$.plotter_machine$preparePlots()

            self$results$.setModel(private$.runner_machine$estimator$model)

            jinfo("MODULE: jAMM  #### phase end ####")
            jinfo("RUN TIME:", Sys.time() - runnow, " secs")
            jinfo("TIME:", Sys.time() - private$.time, " secs")
        },
        .showModelDiagram = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (!is.null(state$modelDiagram))
                state <- new_plot_state(state$modelDiagram)
            render_plot_state(state, plot_model_diagram)
        },
        .showDiagram = function(image, ggtheme, theme, ...) {
            render_plot_state(image$state, draw_path_diagram)
        },
        
        .marshalFormula = function(formula, data, name) {
            formula <- lapply(formula, expand.formula)

            if (name == "simpleScale") {
                return("mean_sd")
            }
            deps <- sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "lhs"))
            ivs <- unique(unlist(sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "rhs"))))
            factors <- unique(unlist(sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "rhs", permitted = "factor"))))
            dep <- setdiff(deps, ivs)
            meds <- intersect(deps, ivs)
            # Covariates are the continuous independent variables: mediators are
            # predictors in the full model too, but they are not covariates.
            covs <- setdiff(ivs, c(factors, meds))

            if (name == "dep") {
                if (length(dep) > 1) {
                    jmvcore::reject("The models in formula imply more than one dependent variable. Please refine your models")
                }
                return(dep)
            }
            if (name == "factors") {
                return(factors)
            }
            if (name == "covs") {
                return(covs)
            }

            if (name == "mediators") {
                if (length(meds) == 0) {
                    jmvcore::reject("The models in formula imply no mediator. Please refine your models")
                }
                return(meds)
            }
            if (name == "mediatorsTerms") {
                medsterms <- lapply(formula, function(equation) {
                    response <- jmvcore::marshalFormula(equation, data, from = "lhs")
                    terms <- NULL
                    if (response %in% meds) {
                        terms <- jmvcore::marshalFormula(equation, data, from = "rhs", type = "terms")
                    }
                    terms
                })
                medsterms <- medsterms[-which(sapply(medsterms, is.null))]
                return(medsterms)
            }

            if (name == "modelTerms") {
                for (equation in formula) {
                    response <- jmvcore::marshalFormula(equation, data, from = "lhs")
                    variables <- jmvcore::marshalFormula(equation, data, from = "rhs")
                    predictors <- unique(unlist(variables))
                    if (response == dep & length(intersect(meds, predictors)) > 0) {
                        return(jmvcore::marshalFormula(equation, data, from = "rhs", type = "terms"))
                    }
                }
            }
        },
        .formula = function() {
            if (is.null(private$.runner_machine)) {
                return("")
            }
            models <- private$.runner_machine$estimator$models()
            paste0("list(", paste(sapply(models, function(model) paste0('"', model$value, '"')), collapse = ","), ")")
        },
        .sourcifyOption = function(option) {
            value <- option$value
           
          ## mod_terms is weired, hanlde it here
           if (option$name=="mod_terms") {
             test<-all(sapply(value,function(a) length(a)==0))
             if (test) return('')
             
             meds<-self$options$mediators
             mods<-character()
             for (i in seq_along(meds)) {
               if (length(value[[i]])>0 ) {
                 variables <- paste(quote_name(value[[i]]), collapse = ", ")
                 mods <- c(mods,paste0(quote_name(meds[[i]]), " = c(", variables, ")"))
               }
             }
             
             mods <- paste0("mod_terms = list(", paste(mods, collapse = ", "), ")")
             return(mods)
           }


            if (option$name %in% NO_R_OPTS) 
              return("")

            # LEGACY (jAMM < 2.0): never write the deprecated option into generated
            # syntax; its value is already in `mod_terms`. Remove with R/legacy.R.
            if (option$name == "moderatorsTerms")
              return("")
            
            # since OptionsArray has not embedded default, we pass them as ARRAY_DEFAULTS defined
            # in constants.R. Other option classes ignore it.
            
            return(sourcifyOption(option, ARRAY_DEFAULTS))
            
            

        }
    )
)
