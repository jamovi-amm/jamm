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

            private$.ready <- readiness(self$options)
            if (!private$.ready$ready) {
                if (private$.ready$report) {
                    self$results$infotab$addRow("info", list(info = "Setup", specs = private$.ready$reason))
                }
                return()
            }

            # Set up the R6 workhorse classes, retaining jAMM's data API.
            dispatch_message_cleaner(self)
            data_machine <- Datamatic$new(self$options, self$data)
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
            aSmartObj$ci("", self$options$ciWidth)
            ladd(private$.smartObjs) <- aSmartObj

            # Indirect and total effects.
            self$results$models$coefficients$getColumn("condition")$setVisible(length(unlist(self$options$moderatorsTerms)) > 0)
            aSmartObj <- SmartTable$new(self$results$models$coefficients, runner_machine)
            aSmartObj$ci("", self$options$ciWidth)
            ladd(private$.smartObjs) <- aSmartObj

            # Contrast definitions start hidden when no factors are specified.
            aSmartObj <- SmartTable$new(self$results$models$contrastCodeTable, runner_machine)
            aSmartObj$activateOnData <- TRUE
            ladd(private$.smartObjs) <- aSmartObj

            if ("regression" %in% self$options$tableOptions) {
                self$results$regressions$setTitle("Regressions Results")
                if (length(self$options$mediators) > 1) {
                    self$results$regressions$mediator_regressions$setTitle("Mediators Models")
                }

                # Total-effects ANOVA and regression tables.
                aSmartObj <- SmartTable$new(self$results$regressions$overall$anova, runner_machine)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$regressions$overall$regression, runner_machine)
                aSmartObj$title <- paste("Total effects predicting:", self$options$dep)
                ladd(private$.smartObjs) <- aSmartObj

                # Each mediator owns a group containing ANOVA and coefficients.
                aSmartObj <- SmartArray$new(self$results$regressions$mediator_regressions, runner_machine)
                ladd(private$.smartObjs) <- aSmartObj
                mediatorsSmartObj <- aSmartObj

                # Full-model ANOVA and regression tables.
                aSmartObj <- SmartTable$new(self$results$regressions$full$anova, runner_machine)
                ladd(private$.smartObjs) <- aSmartObj

                aSmartObj <- SmartTable$new(self$results$regressions$full$regression, runner_machine)
                aSmartObj$title <- paste("Full model predicting", self$options$dep)
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
            plotter_machine <- Plotter$new(self$options, data_machine, runner_machine$estimator, self$results$pathgroup)
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

            # Run tables through SmartTabs, never by calling the providers here.
            for (smarttab in private$.smartObjs) {
                smarttab$runTable()
            }

            # Preserve the existing diagram preparation and warning reporting.
            private$.plotter_machine$preparePlots()
            if (is.something(private$.plotter_machine$warnings$diagram)) {
                for (index in seq_along(private$.plotter_machine$warnings$diagram)) {
                    self$results$pathgroup$notes$addRow(index, list(message = private$.plotter_machine$warnings$diagram[[index]]))
                }
                self$results$pathgroup$notes$setVisible(TRUE)
            }

            self$results$.setModel(private$.runner_machine$estimator$model)

            jinfo("MODULE: jAMM  #### phase end ####")
            jinfo("RUN TIME:", Sys.time() - runnow, " secs")
            jinfo("TIME:", Sys.time() - private$.time, " secs")
        },
        # This callback matches pathmodel's renderFun, not the semPlot images.
        .showModelDiagram = function(image, ggtheme, theme, ...) {
            if (!self$options$meddiagram || is.null(image$state$modelDiagram)) {
                return()
            }
            plot_model_diagram(image$state$modelDiagram)
            return(TRUE)
        },
        .showDiagram = function(image, ggtheme, theme, ...) {
            if (self$options$diagram == FALSE) {
                return()
            }
            if (!is.something(image$state$semModel)) {
                return()
            }
            options <- private$.plotter_machine$semPathsOptions
            res <- try_hard(
                semPlot::semPaths(
                    object = image$state$semModel,
                    layout = options$layout,
                    residuals = options$residuals,
                    rotation = options$rotation,
                    intercepts = options$intercepts,
                    nodeLabels = options$nodeLabels,
                    whatLabels = options$whatLabels,
                    sizeMan = options$sizeMan,
                    sizeMan2 = options$sizeMan2,
                    curve = options$curve,
                    shapeMan = options$shapeMan,
                    edge.label.cex = options$edge.label.cex
                )
            )
            note <- FALSE

            if (!isFALSE(res$error)) {
                if (length(grep("Circle layout only supported", res$error, fixed = TRUE)) > 0) {
                    res$error <- PLOT_WARNS[["nocircle"]]
                    note <- TRUE
                }
                if (length(grep("graph_from_edgelist", res$error, fixed = TRUE)) > 0) {
                    res$error <- PLOT_WARNS[["nocircle"]]
                    note <- TRUE
                }
                if (length(grep("subscript out of", res$error, fixed = TRUE)) > 0) {
                    res$error <- PLOT_WARNS[["fail"]]
                    note <- TRUE
                }
            }

            if (!isFALSE(res$error)) {
                self$results$pathgroup$notes$addRow("err", list(message = res$error))
                note <- TRUE
            }
            if (!isFALSE(res$warning)) {
                self$results$pathgroup$notes$addRow("war", list(message = res$warning))
                note <- TRUE
            }

            if (note) {
                self$results$pathgroup$notes$setVisible(TRUE)
            }

            return(TRUE)
        },
        .marshalFormula = function(formula, data, name) {
            formula <- lapply(formula, expand.formula)

            if (name == "simpleScale") {
                return("mean_sd")
            }
            deps <- sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "lhs"))
            ivs <- unique(unlist(sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "rhs"))))
            factors <- unique(unlist(sapply(formula, function(equation) jmvcore::marshalFormula(equation, data, from = "rhs", permitted = "factor"))))
            covs <- setdiff(ivs, factors)
            dep <- setdiff(deps, ivs)
            meds <- intersect(deps, ivs)

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
            name <- option$name
            value <- option$value

            if (!is.something(value)) {
                return("")
            }
            if (name %in% c("factors", "endogenous", "covs", "endogenousTerms")) {
                return("")
            }
            if (name == "scaling") {
                vec <- sourcifyList(option, "none")
                return(vec)
            }
            if (name == "contrasts") {
                vec <- sourcifyList(option, "simple")
                return(vec)
            }
            if (name == "varcov") {
                vec <- lapply(self$options$varcov, function(covariance) c(covariance$i1, covariance$i2))
                vec <- paste0("varcov=list(", paste(vec, collapse = ","), ")", collapse = "")
                return(vec)
            }

            super$.sourcifyOption(option)
        }
    )
)
