ERROR_TABLE="jamovi"

ARRAY_DEFAULTS <- c(scaling = "centered", contrasts = "simple")
NO_R_OPTS <- c("mediators","mediatorsTerms","covs","factors","dep","modelTerms")
  


WARNS<-list()
WARNS[["nocenterd"]]<-"Variables {vars} are not centered. Consider using the Covariate Scaling options for easier interpretation of lower-order effects."
WARNS[["noindirect"]]<-"Indirect effects cannot be computed for this model."
WARNS[["fewboot"]]<-"Bootstrap confidence intervals are based on {n} bootstrap samples. Increasing the number of bootstrap samples to at least 5000 is suggested, for more stable intervals."
WARNS[["bootci"]]<-"Confidence intervals are computed with the bootstrap method ({type}, {n} bootstrap samples)."
WARNS[["fixeddirect"]]<-"Direct effects with estimate 0 and no standard error are not in the full model: they are fixed to 0, not estimated."

DATA_WARNS<-list()
# {var} is replaced by the variable name. jamovi passes ordinal variables as
# ordered factors and nominal variables as factors (see Datamatic$cleandata()).
DATA_WARNS[["ord_to_cont"]]<-"{var} is an ordinal variable: it is used as a continuous variable, with its numeric values."
DATA_WARNS[["fac_to_cont"]]<-"{var} is a nominal variable: it is used as a continuous variable, with its numeric values. Please make sure that this is intended."

ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model."

# Description shown next to the estimator's code in the Models Info table.
estimator_description <- function(estimator) {
  descriptions <- c(ML = "Maximum likelihood")
  if (estimator %in% names(descriptions)) descriptions[[estimator]] else ""
}
