
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


  if(length(options$dep) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need a dependent variable")
    return(result)
  } 

  if(length(options$mediators) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 mediator variable")
    return(result)
  } 
  
  if((length(options$factors) == 0) & (length(options$covs) == 0)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 independent variable")
    return(result)
  } 
  
  check<-sum(unlist(sapply(options$mediatorsTerms, function(l) as.numeric(length(l)>0))))
  if (check< length(options$mediators)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Predictors not specified for {length(options$mediators)-check} mediator variable")
    return(result)
  }

  if (length(options$modelTerms)==0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("The full model is not specified")
    return(result)
  }
  
  return(result)
}
