
#' GLM Mediation Model
#'
#' GLM mediation model
#' @param formula a list of formulas to use, see the examples
#' @param data the data as a data frame
#' @param mediatorsTerms a list of lists specifying the models for with the
#'   mediators as dependent variables. Not required in formula is used.
#' @param moderatorsTerms a named list of the form list("med"=c("mod1",mod2"),med2="mod1") specifying the moderator(s) of each mediator. 
#'           This is required to decide for which variable we need to condition the mediated effects and to single out moderators
#'           in the path diagram. If not specified, any interaction is considered as any other term in the model 
#' @param dep a string naming the dependent variable from \code{data},
#'   variable must be numeric. Not useful if formula is used.
#' @param mediators a vector of strings naming the mediators from \code{data}. Not useful if formula is used.
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}. Not useful if formula is used or the variable is already a factor in the data
#' @param covs a vector of strings naming the covariates from \code{data}. Not useful if formula is used.
#' @param modelTerms a list of character vectors describing fixed effects  terms. Not useful if formula is used
#' @param ciType Choose the confidence interval type
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the parameter estimates
#' @param bootN number of bootstrap samples for estimating confidence
#'   intervals
#' @param contrasts a named vector of the form \code{c(var1='type', var2='type2')} specifying the type of contrast to use,
#'    one of \code{'deviation'}, \code{'simple'}, \code{'dummy'}, \code{'difference'}, \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}.
#'     If NULL, \code{'simple'} is used. Can also be passed as a list of list of the form \code{list(list(var='var1',type='type1'))}.
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), provide raw
#'   names of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param simpleScale \code{'mean_sd'} (default), \code{'custom'} , or
#'   \code{'custom_percent'}. Use to condition the covariates (if any).
#' @param cvalue offset value for conditioning. Values are mean +/- cvalue. 
#' @param percvalue offset value for conditioning. Values are median +/- pecvalue
#' @param simpleScaleLabels style for presenting condition values of a moderator. It can be \code{'labels'} (default), \code{'values'} or \code{'labels_values'} for both.   
#' @param scaling a named vector of the form \code{c(var1='type1',var2='type2')}. Types are
#'   \code{'centered'} to the mean, \code{'standardized'}, log-transformed \code{'log'} or \code{'none'}.
#'   \code{'none'} leaves the variable as it is. It can also be passed as a list of lists.

#' @param tableOptions a vector of options to be shown in the tables. \code{'component'} shows the indirect effects 
#' components in the results tables. \code{'regression'} show the results of all the regression (R-squared, F-test and coefficients) involved in the model.
#' @param pathOptions .
#' @param bogus \code{a bogus option to define a label without visible
#'   children, used for internal checks}

#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pathmodelgroup$pathmodel} \tab \tab \tab \tab \tab a path model \cr
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
  bogus = FALSE,
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
  formula) {
  
  if ( ! requireNamespace('jmvcore'))
    stop('jammGLM requires jmvcore to be installed (restart may be required)')
  
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
  if (inherits(modelTerms, 'formula')) modelTerms <- jmvcore::decomposeFormula(modelTerms)
  
  ## fix some option when passed by R console ###

  if (is.something(names(moderatorsTerms))) {
    if (!is.list(moderatorsTerms))
      jmvcore::reject("The `modelTerms` option should be a list")
     
     moderatorsTerms<-lapply(mediators, function(med) {
        ifelse(med %in% names(moderatorsTerms),as.list(moderatorsTerms[med]),list())
     })
  }
  mark(moderatorsTerms)
  if (is.something(names(scaling))) 
    scaling<-lapply(names(scaling), function(a) list(var=a,type=scaling[[a]]))
  if (is.something(names(contrasts))) 
    contrasts<-lapply(names(contrasts), function(a) list(var=a,type=contrasts[[a]]))
  
  ############
  ### some check for input passed by R console ###
  moderators<-unlist(moderatorsTerms)
  test1<-intersect(moderators,mediators)
  if (length(test1)>0)
    jmvcore::reject(paste("You specified the variable", test1,"to be both a mediator and a moderator. This is not allowed. You can specify interactions
                          involving moderators and mediators as you like, but do not assign the role of moderator to a mediator."))
  
  
  
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
    bogus = bogus,
    simpleScale = simpleScale,
    cvalue = cvalue,
    percvalue = percvalue,
    simpleScaleLabels = simpleScaleLabels,
    scaling = scaling,
    tableOptions = tableOptions,
    pathOptions = pathOptions,
    mediatorsTerms = mediatorsTerms,
    moderatorsTerms = moderatorsTerms)

  
  
  
  analysis <- jammGLMClass$new(
    options = options,
    data = data)
  analysis$run()
  
  analysis$results
}
