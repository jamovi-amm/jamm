# Estimation is separate from the run_* data providers dispatched by SmartTabs.
Runner <- R6::R6Class(
    "Runner",
    inherit = Initer,
    cloneable = FALSE,
    public = list(
        data = NULL,
        estimate = function(data) {
            self$ok <- TRUE
            self$data <- data
            private$.regressions <- list()
            self$estimator$estimate(data)
            if (is.something(self$estimator$errors)) {
                # Report through Scaffold and retain lavaan's diagnostic details.
                self$ok <- FALSE
                self$warning <- list(
                    topic = "issues",
                    message = paste("Model cannot be estimated:", paste(self$estimator$errors, collapse = "; ")),
                    head = "error"
                )
                return(invisible(NULL))
            }
        },
        run_infotab = function() {
            # SmartTable catches warnings and attaches them to the owning table.
            for (message in self$estimator$warnings[["main"]])
                warning(message, call. = FALSE)
            c(table_rows(self$estimator$tab_info), table_rows(self$estimator$models()),
              table_rows(self$estimator$tab_indirect))
        },
        run_models_r2 = function() {
            for (message in self$estimator$warnings[["r2"]])
                warning(message, call. = FALSE)
            table_rows(self$estimator$tab_r2)
        },
        run_models_interactions = function() {
            table_rows(self$estimator$tab_interactions)
        },
        run_models_coefficients = function() {
            table_rows(self$estimator$tab_coefficients)
        },
        run_models_contrastCodeTable = function() {
            self$init_models_contrastCodeTable()
        },
        run_models_contrastCodeTables = function() {
            self$init_models_contrastCodeTables()
        },
        run_regressions_overall_anova = function() {
            private$.regression_values("overall", self$options$dep, private$.total_terms(), total = TRUE)$anova
        },
        run_regressions_overall_regression = function() {
            private$.regression_values("overall", self$options$dep, private$.total_terms(), total = TRUE)$regression
        },
        run_regressions_mediator_regressions = function() {
            rows <- lapply(seq_along(self$options$mediators), function(index) {
                private$.regression_values(paste0("mediator_", index), self$options$mediators[[index]],
                                           self$options$mediatorsTerms[[index]])
            })
            stats::setNames(rows, self$options$mediators)
        },
        run_regressions_full_anova = function() {
            private$.regression_values("full", self$options$dep, self$options$modelTerms)$anova
        },
        run_regressions_full_regression = function() {
            private$.regression_values("full", self$options$dep, self$options$modelTerms)$regression
        }
    ),
    private = list(
        .regressions = list(),
        .regression_values = function(key, dep, terms, total = FALSE) {
            # ANOVA and coefficient providers share one fit per equation.
            if (is.null(private$.regressions[[key]]))
                private$.regressions[[key]] <- regression_values(dep, terms, self$estimator,
                                                               self$data, self$options$ciWidth / 100, total)
            private$.regressions[[key]]
        }
    )
)
