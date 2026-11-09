# Moderator settings use the same transformed columns and contrast coding as
# the fitted model, rather than assuming that every moderator is centered.
moderator_settings <- function(options, datamatic, data = NULL) {
    moderators <- unique(unlist(options$moderatorsTerms))
    settings <- lapply(moderators, function(moderator) {
        if (moderator %in% options$factors) {
            levels <- datamatic$factors_levels[[moderator]]
            coding <- datamatic$contrasts_values[[moderator]]
            columns <- paste0(tob64(moderator), FACTOR_SYMBOL, seq_len(ncol(coding)))
            return(lapply(seq_along(levels), function(index) {
                list(values = stats::setNames(as.numeric(coding[index, ]), columns),
                     label = paste(moderator, levels[[index]], sep = " = "))
            }))
        }
        column <- tob64(moderator)
        if (is.null(data)) {
            values <- c(-1, 0, 1)
        } else if (options$simpleScale == "percent") {
            offset <- options$percvalue / 100
            values <- as.numeric(stats::quantile(data[[column]], c(.5 - offset, .5, .5 + offset)))
        } else {
            values <- mean(data[[column]]) + c(-1, 0, 1) * options$cvalue * stats::sd(data[[column]])
        }
        labels <- if (options$simpleScale == "percent") {
            paste0(c(50 - options$percvalue, 50, 50 + options$percvalue), "%")
        } else {
            c(paste0("Mean - ", options$cvalue, " SD"), "Mean", paste0("Mean + ", options$cvalue, " SD"))
        }
        if (options$simpleScaleLabels == "values")
            labels <- format(round(values, 3), trim = TRUE)
        if (options$simpleScaleLabels == "values_labels")
            labels <- paste(labels, format(round(values, 3), trim = TRUE), sep = " = ")
        lapply(seq_along(values), function(index) {
            list(values = stats::setNames(values[[index]], column),
                 label = paste(moderator, labels[[index]], sep = " = "))
        })
    })
    combinations <- expand.grid(lapply(settings, seq_along), KEEP.OUT.ATTRS = FALSE)
    lapply(seq_len(nrow(combinations)), function(index) {
        selected <- lapply(seq_along(settings), function(moderator) {
            settings[[moderator]][[combinations[index, moderator]]]
        })
        list(values = unlist(lapply(selected, function(setting) setting$values)),
             label = paste(vapply(selected, function(setting) setting$label, character(1)), collapse = "; "))
    })
}

# Each conditional slope includes its main coefficient plus every interaction
# with the selected moderators. Products of these slopes define indirect paths;
# totals sum all indirect paths sharing endpoints and the conditional direct path.
conditional_effects <- function(options, datamatic, parameters, chains, data = NULL) {
    if (length(chains) == 0)
        return(list())
    regressions <- parameters[parameters$op == "~", ]
    terms <- strsplit(regressions$rhs, INTERACTION_SYMBOL, fixed = TRUE)
    slope <- function(from, to, values) {
        selected <- which(regressions$lhs == to & vapply(terms, function(term) {
            sum(term == from) == 1 && all(term[term != from] %in% names(values))
        }, logical(1)))
        if (length(selected) == 0)
            return("0")
        summands <- vapply(selected, function(index) {
            modifiers <- setdiff(terms[[index]], from)
            multiplier <- prod(values[modifiers])
            paste0(regressions$label[[index]], "*(", sprintf("%.17g", multiplier), ")")
        }, character(1))
        paste0("(", paste(summands, collapse = "+"), ")")
    }
    rows <- list()
    add <- function(type, effect, expression, condition, from, to) {
        scale <- if (is.null(data)) NA_real_ else stats::sd(data[[from]]) / stats::sd(data[[to]])
        rows[[length(rows) + 1]] <<- list(
            parameter = paste0("CE", length(rows) + 1), type = type, effect = effect,
            expression = expression, condition = condition, scale = scale)
    }
    endpoints <- vapply(chains, function(chain) paste(chain[[1]], tail(chain, 1), sep = ":"), character(1))
    for (setting in moderator_settings(options, datamatic, data)) {
        products <- character(length(chains))
        for (index in seq_along(chains)) {
            chain <- chains[[index]]
            components <- vapply(seq_len(length(chain) - 1), function(edge) {
                slope(chain[[edge]], chain[[edge + 1]], setting$values)
            }, character(1))
            products[[index]] <- paste(components, collapse = "*")
            add("Indirect", paste(fromb64(chain), collapse = " \U21d2 "), products[[index]],
                setting$label, chain[[1]], tail(chain, 1))
            if ("component" %in% options$tableOptions) {
                for (edge in seq_along(components)) {
                    add("Component", paste(fromb64(chain[c(edge, edge + 1)]), collapse = " \U21d2 "),
                        components[[edge]], setting$label, chain[[edge]], chain[[edge + 1]])
                }
            }
        }
        for (endpoint in unique(endpoints)) {
            matching <- which(endpoints == endpoint)
            chain <- chains[[matching[[1]]]]
            from <- chain[[1]]
            to <- tail(chain, 1)
            label <- paste(fromb64(c(from, to)), collapse = " \U21d2 ")
            direct <- slope(from, to, setting$values)
            add("Direct", label, direct, setting$label, from, to)
            add("Total", label, paste(c(direct, products[matching]), collapse = "+"),
                setting$label, from, to)
        }
    }
    rows
}
