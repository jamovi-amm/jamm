#' GLM Mediation Model
#'
#' GLM mediation model
#' @param formula a single string of equations separated by semicolons or newlines,
#'   or a list containing one equation per element as a string or formula object.
#' @param data the data as a data frame
#' @param dep a string naming the dependent variable from \code{data},
#'   variable must be numeric
#' @param mediators a vector of strings naming the mediators from \code{data}
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms
#' @param ciType confidence interval type: \code{'standard'} (default, delta
#'   method), \code{'bca'} (bias-corrected bootstrap), \code{'perc'}
#'   (percentile bootstrap), \code{'norm'} (normal bootstrap), or \code{'none'}.
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the parameter estimates
#' @param bootN integer number of bootstrap samples (at least 50) for estimating
#'   confidence intervals
#' @param contrasts a list of lists specifying the factor and type of contrast
#'   to use, one of \code{'simple'}, \code{'deviation'}, \code{'dummy'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}
#' @param showRealNames \code{TRUE} (default) or \code{FALSE}; show the original
#'   variable names in contrast estimates.
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param simpleScale \code{'mean_sd'} (default) uses the mean and offsets of
#'   \code{cvalue} standard deviations; \code{'percent'} uses the median and
#'   percentile offsets of \code{percvalue} for continuous moderators.
#' @param cvalue standard-deviation offset for conditioning (at least 0.1)
#' @param percvalue percentile offset from the median, between 5 and 50 (default: 25).
#' @param simpleScaleLabels moderator setting labels: \code{'labels'} (default),
#'   \code{'values'}, or \code{'values_labels'}.
#' @param scaling a list of lists specifying the covariates scaling, one of
#'   \code{'centered'}, \code{'standardized'}, or \code{'none'}.
#'   \code{'none'} leaves the variable as it is
#' @param tableOptions optional table content: \code{'beta'} and \code{'component'}
#'   (both selected by default), and \code{'regression'}.
#' @param pathOptions include \code{'suggested'} (default) to show suggested
#'   paths in the conceptual model diagram without changing the fitted model.
#' @param mediatorsTerms a list of lists specifying the models with the
#'   mediators as dependent variables.
#' @param mod_terms a list of lists specifying the variables that
#'   moderate each mediated effect.
#'   Categorical moderators include each factor level and an Average setting
#'   at the equal-level contrast centroid (zero for centered factor coding).
#'   Indirect effects at Average are products of the centered-factor slopes,
#'   not averages of the level-specific indirect effects.
#' @param moderatorsTerms deprecated, use \code{mod_terms}. Accepted (with a
#'   warning) for scripts written for jAMM 1.x; ignored when \code{mod_terms}
#'   is also given.
#' @param model_diagram \code{TRUE} (default) or \code{FALSE}; show the conceptual
#'   model diagram based on the specified regression terms.
#' @param path_diagram \code{TRUE} or \code{FALSE} (default); show the statistical
#'   path diagram based on the fitted model.
#' @param diag_paths statistical path labels: \code{'est'} (default) for
#'   coefficients, \code{'stand'} for standardized coefficients, \code{'name'}
#'   for parameter labels, or \code{'hide'} for no labels.
#' @param diag_resid \code{TRUE} or \code{FALSE} (default); show residuals in
#'   the statistical path diagram.
#' @param diag_labsize statistical diagram node size: \code{'small'},
#'   \code{'medium'} (default), \code{'large'}, or \code{'vlarge'}.
#' @param diag_rotate exogenous-node position in the statistical diagram:
#'   \code{'1'} (top, default), \code{'2'} (left), \code{'3'} (bottom), or
#'   \code{'4'} (right). The mediation-like layout uses fixed positions.
#' @param diag_type statistical diagram layout: \code{'mediation'} (default),
#'   \code{'circle'}, \code{'circle2'}, \code{'tree2'}, \code{'tree'}, or \code{'spring'}.
#' @param diag_shape statistical diagram node shape: \code{'rectangle'} (default),
#'   \code{'square'}, \code{'circle'}, \code{'ellipse'}, or \code{'diamond'}.
#' @param diag_abbrev minimum abbreviation length for statistical diagram node
#'   labels: \code{'0'} (default, no abbreviation), \code{'5'}, \code{'10'},
#'   \code{'15'}, \code{'20'}, or \code{'25'}.

#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{lavaan} object \cr
#'   \code{results$issues} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$infotab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$r2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$interactions} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$contrastCodeTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$regressions$overall} \tab \tab \tab \tab \tab a group \cr
#'   \code{results$regressions$mediator_regressions} \tab \tab \tab \tab \tab an array of regressions for the mediators \cr
#'   \code{results$regressions$full} \tab \tab \tab \tab \tab a group \cr
#'   \code{results$pathgroup$pathmodel} \tab \tab \tab \tab \tab a model diagram \cr
#'   \code{results$pathgroup$pathnotes} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pathgroup$diagrams} \tab \tab \tab \tab \tab an array of path diagrams \cr
#'   \code{results$pathgroup$notes} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$infotab$asDF}
#'
#' \code{as.data.frame(results$infotab)}
#'
#' @export
jammGLM <- function(
    formula,
    data,
    dep = NULL,
    mediators = NULL,
    factors = NULL,
    covs = NULL,
    modelTerms = NULL,
    ciType = "standard",
    ciWidth = 95,
    bootN = 1000,
    contrasts = NULL,
    showRealNames = TRUE,
    showContrastCode = FALSE,
    simpleScale = "mean_sd",
    cvalue = 1,
    percvalue = 25,
    simpleScaleLabels = "labels",
    scaling = NULL,
    tableOptions = list(
                "beta",
                "component"),
    pathOptions = list(
                "suggested"),
    mediatorsTerms = list(
                list()),
    mod_terms = list(
                list()),
    model_diagram = TRUE,
    path_diagram = FALSE,
    diag_paths = "est",
    diag_resid = FALSE,
    diag_labsize = "medium",
    diag_rotate = "1",
    diag_type = "mediation",
    diag_shape = "rectangle",
    diag_abbrev = "0",
    # LEGACY (jAMM < 2.0): deprecated alias of `mod_terms`. Remove with R/legacy.R.
    moderatorsTerms = NULL
    ) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jammGLM requires jmvcore to be installed (restart may be required)")


    if ( ! missing(formula)) {

        # formula can be a text (character) or a list of strings
        if (is.character(formula)) {
          if (length(formula) != 1 || is.na(formula))
            stop("'formula' must be a single non-missing string, or a list with one equation per element.", call. = FALSE)
          formula <- stringr::str_split(formula, "[;\r\n]+")[[1]]
          formula <- stringr::str_trim(formula)
          formula <- as.list(formula[nzchar(formula)])
        }
        if (!is.list(formula) || inherits(formula, "formula"))
          stop("'formula' must be a string of equations or a list with one equation per element.", call. = FALSE)
        if (length(formula) == 0)
          stop("'formula' must contain at least one equation.", call. = FALSE)
        formula <- lapply(seq_along(formula), function(index) {
          tryCatch(expand.formula(formula[[index]]), error = function(error) {
            stop(paste0("Invalid formula entry ", index, ": ", conditionMessage(error)), call. = FALSE)
          })
        })

        if (missing(dep))
            dep <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="dep")
        if (missing(factors))
            factors <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="factors")
        if (missing(covs))
            covs <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="covs")
        if (missing(mediators))
            mediators <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="mediators")
        if (missing(mediatorsTerms))
            mediatorsTerms <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="mediatorsTerms")
        if (missing(modelTerms))
            modelTerms <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="modelTerms")
        if (missing(mod_terms))
            mod_terms <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="mod_terms")
        if (missing(simpleScale))
            simpleScale <- jammGLMClass$private_methods$.marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                name="simpleScale")
    }

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(mediators)) mediators <- jmvcore::resolveQuo(jmvcore::enquo(mediators))
    if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
    if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(mediators), mediators, NULL),
            `if`( ! missing(factors), factors, NULL),
            `if`( ! missing(covs), covs, NULL))

    for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    if (inherits(modelTerms, "formula")) modelTerms <- jmvcore::decomposeFormula(modelTerms)

    # LEGACY (jAMM < 2.0): accept scripts written for jAMM 1.x, which named
    # this argument `moderatorsTerms`. Remove together with R/legacy.R.
    if (!is.null(moderatorsTerms)) {
        warning("'moderatorsTerms' is deprecated; use 'mod_terms' instead.", call. = FALSE)
        if (length(unlist(mod_terms)) == 0)
            mod_terms <- moderatorsTerms
    }

    options <- jammGLMOptions$new(
        dep = dep,
        mediators = mediators,
        factors = factors,
        covs = covs,
        modelTerms = modelTerms,
        ciType = ciType,
        ciWidth = ciWidth,
        bootN = bootN,
        contrasts = contrasts,
        showRealNames = showRealNames,
        showContrastCode = showContrastCode,
        simpleScale = simpleScale,
        cvalue = cvalue,
        percvalue = percvalue,
        simpleScaleLabels = simpleScaleLabels,
        scaling = scaling,
        tableOptions = tableOptions,
        pathOptions = pathOptions,
        mediatorsTerms = mediatorsTerms,
        mod_terms = mod_terms,
        model_diagram = model_diagram,
        path_diagram = path_diagram,
        diag_paths = diag_paths,
        diag_resid = diag_resid,
        diag_labsize = diag_labsize,
        diag_rotate = diag_rotate,
        diag_type = diag_type,
        diag_shape = diag_shape,
        diag_abbrev = diag_abbrev)

    analysis <- jammGLMClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

