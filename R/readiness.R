
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


  if(length(options$dep) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "A dependent variable is needed."
    return(result)
  } 

  if(length(options$mediators) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "At least one mediator variable is needed."
    return(result)
  } 
  
  if((length(options$factors) == 0) & (length(options$covs) == 0)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "At least one independent variable is needed."
    return(result)
  } 
  
  check<-sum(unlist(sapply(options$mediatorsTerms, function(l) as.numeric(length(l)>0))))
  if (check< length(options$mediators)) {
    result$ready <- FALSE
    result$report <- TRUE
    missing <- length(options$mediators) - check
    result$reason <- sprintf("Predictors are not specified for %s mediator %s.", missing,
                            if (missing == 1) "variable" else "variables")
    return(result)
  }

  # UI restoration or direct R calls must not bypass the predictor-role rule.
  predictors <- unique(c(options$factors, options$covs))
  moderators <- unique(unlist(options$moderatorsTerms))
  if (length(setdiff(predictors, moderators)) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "At least one independent variable must remain outside the moderators."
    return(result)
  }

  if (length(options$modelTerms)==0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "The full model is not specified."
    return(result)
  }
  
  return(result)
}
