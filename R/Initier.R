# SmartTable/SmartArray call these methods to obtain layouts, never result objects.
Initer <- R6::R6Class(
    "Initer",
    inherit = Scaffold,
    cloneable = FALSE,
    public = list(
        datamatic = NULL,
        estimator = NULL,
        vars = NULL,
        initialize = function(jmvobj, datamatic) {
            super$initialize(jmvobj)
            self$datamatic <- datamatic
            self$estimator <- Estimate$new(self$options, datamatic)
            self$vars <- self$estimator$vars
        },
        init_infotab = function() {
            c(table_rows(self$estimator$tab_info), table_rows(self$estimator$models()),
              table_rows(self$estimator$tab_indirect))
        },
        init_models_r2 = function() {
            lapply(self$estimator$tab_r2, function(variable) list(lhs = variable))
        },
        init_models_interactions = function() {
            table_rows(self$estimator$tab_interactions)
        },
        init_models_coefficients = function() {
            table_rows(self$estimator$tab_coefficients)
        },
        init_models_contrastCodeTable = function() {
            rows <- list()
            for (factor in self$options$factors) {
                labels <- self$datamatic$contrasts_labels[[factor]]
                for (index in seq_along(labels))
                    rows[[length(rows) + 1]] <- list(rname = paste0(factor, index), clab = labels[[index]])
            }
            if (length(rows) > 0) rows else NULL
        },
        init_regressions_overall_anova = function() {
            list(list(dep = self$options$dep))
        },
        init_regressions_overall_regression = function() {
            regression_layout(self$options$dep, private$.total_terms(), self$estimator, total = TRUE)$regression
        },
        init_regressions_mediator_regressions = function() {
            rows <- lapply(seq_along(self$options$mediators), function(index) {
                regression_layout(self$options$mediators[[index]], self$options$mediatorsTerms[[index]], self$estimator)
            })
            stats::setNames(rows, self$options$mediators)
        },
        init_regressions_full_anova = function() {
            list(list(lhs = self$options$dep))
        },
        init_regressions_full_regression = function() {
            regression_layout(self$options$dep, self$options$modelTerms, self$estimator)$regression
        }
    ),
    private = list(
        .total_terms = function() {
            Filter(function(term) length(intersect(self$options$mediators, term)) == 0, self$options$modelTerms)
        }
    )
)
