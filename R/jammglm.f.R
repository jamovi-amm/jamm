# ## fix some option when passed by R console ###
## this code should be added in the jammGLM function generated
## by jamovi compiler after copying it from .h.R to .f.R
# 
# >> for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
# >> if (inherits(modelTerms, 'formula')) modelTerms <- jmvcore::decomposeFormula(modelTerms)
# 
# 
# if (is.something(names(moderatorsTerms))) {
#   if (!is.list(moderatorsTerms))
#     jmvcore::reject("The `moderatorsTerms` option should be a list")
#   
#   moderatorsTerms<-lapply(mediators, function(med) {
#     ifelse(med %in% names(moderatorsTerms),as.list(moderatorsTerms[med]),list())
#   })
# }
# if (is.something(names(scaling))) 
#   scaling<-lapply(names(scaling), function(a) list(var=a,type=scaling[[a]]))
# if (is.something(names(contrasts))) 
#   contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))
# 
# ############
# ### some check for input passed by R console ###
# moderators<-unlist(moderatorsTerms)
# test1<-intersect(moderators,mediators)
# if (length(test1)>0)
#   jmvcore::reject(paste("You specified the variable", test1,"to be both a mediator and a moderator. This is not allowed. You can specify interactions involving moderators and mediators as you like, but do not assign the role of moderator to a mediator."))





#' GLM Mediation Model
#'
#' GLM mediation model
#' @param data the data as a data frame
#' @param dep a string naming the dependent variable from \code{data},
#'   variable must be numeric
#' @param mediators a vector of strings naming the mediators from \code{data}
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param modelTerms a list of character vectors describing fixed effects
#'   terms
#' @param ciType Choose the confidence interval type
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the parameter estimates
#' @param bootN number of bootstrap samples for estimating confidence
#'   intervals
#' @param contrasts a list of lists specifying the factor and type of contrast
#'   to use, one of \code{'deviation'}, \code{'simple'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), provide raw
#'   names of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param simpleScale \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'custom_percent'}. Use to condition the covariates (if any)
#' @param cvalue offset value for conditioning
#' @param percvalue offset value for conditioning
#' @param simpleScaleLabels .
#' @param scaling a list of lists specifying the covariates scaling, one of
#'   \code{'centered to the mean'}, \code{'standardized'}, or \code{'none'}.
#'   \code{'none'} leaves the variable as it is
#' @param tableOptions .
#' @param pathOptions .
#' @param mediatorsTerms a list of lists specifying the models for with the
#'   mediators as dependent variables.
#' @param moderatorsTerms a list of lists specifying the the IV which
#'   moderatorate each mediated effect.
#' @param missing handle  missing values: \code{listwise} or \code{full information max likelihood}.
#' @param diagram Choose the type of diagram: \code{conceptual} or
#'   \code{statistical}.
#' @param diag_paths Choose the diagram labels
#' @param diag_labsize Choose the diagram labels
#' @param diag_shape Choose the diagram labels
#' @param diag_abbrev Choose the diagram labels
#' @param diag_offset \code{TRUE} or \code{FALSE} (default), offset labels in
#'   diagram
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pathmodelgroup$pathmodel} \tab \tab \tab \tab \tab a path model \cr
#'   \code{results$pathmodelgroup$statmodel} \tab \tab \tab \tab \tab a path model \cr
#'   \code{results$pathmodelgroup$pathnotes} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$moderationEffects} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$main} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$contrastCodeTables} \tab \tab \tab \tab \tab an array of contrast coefficients tables \cr
#'   \code{results$regressions$overall} \tab \tab \tab \tab \tab a group \cr
#'   \code{results$regressions$mediator_regressions} \tab \tab \tab \tab \tab an array of regressions for the mediators \cr
#'   \code{results$regressions$full} \tab \tab \tab \tab \tab a group \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
jammGLM <- function(
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
    moderatorsTerms = list(
      list()),
    missing = "listwise",
    diagram = "conceptual",
    diag_paths = "est",
    diag_labsize = "medium",
    diag_shape = "rectangle",
    diag_abbrev = "0",
    diag_offset = FALSE,
    formula) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("jammGLM requires jmvcore to be installed (restart may be required)")
  
  if ( ! missing(formula)) {
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
    if (missing(moderatorsTerms))
      moderatorsTerms <- jammGLMClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="moderatorsTerms")
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

## fix some option when passed by R console ###
# this code should be added in the jammGLM function generated
# by jamovi compiler after copying it from .h.R to .f.R



      if (is.something(names(moderatorsTerms))) {
        if (!is.list(moderatorsTerms))
          jmvcore::reject("The `moderatorsTerms` option should be a list")

        moderatorsTerms<-lapply(mediators, function(med) {
          ifelse(med %in% names(moderatorsTerms),as.list(moderatorsTerms[med]),list())
        })
      }
      if (is.something(names(scaling)))
        scaling<-lapply(names(scaling), function(a) list(var=a,type=scaling[[a]]))
      if (is.something(names(contrasts)))
        contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))

############
### some check for input passed by R console ###
      moderators<-unlist(moderatorsTerms)
      test1<-intersect(moderators,mediators)
      if (length(test1)>0)
        jmvcore::reject(paste("You specified the variable", test1,"to be both a mediator and a moderator. This is not allowed. You can specify interactions involving moderators and mediators as you like, but do not assign the role of moderator to a mediator."))

    
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
    moderatorsTerms = moderatorsTerms,
    missing= missing,
    diagram = diagram,
    diag_paths = diag_paths,
    diag_labsize = diag_labsize,
    diag_shape = diag_shape,
    diag_abbrev = diag_abbrev,
    diag_offset = diag_offset)
  
  analysis <- jammGLMClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}

