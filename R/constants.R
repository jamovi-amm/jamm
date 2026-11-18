ERROR_TABLE="jamovi"

INTERACTION_SYMBOL="__XX__XX__"
FACTOR_SYMBOL="._._._."

ARRAY_DEFAULTS <- c(scaling = "centered", contrasts = "simple")
NO_R_OPTS <- c("mediators","mediatorsTerms","covs","factors","dep","modelTerms")
  


WARNS<-list()
WARNS[["nocenterd"]]<-"Variables {vars} are not centered. Consider using the Covariate Scaling options for easier interpretation of lower-order effects."
WARNS[["noindirect"]]<-"Indirect effects cannot be computed for this model."

DATA_WARNS<-list()
DATA_WARNS[["fac_to_cont"]]<-"Warning: a continuous variable is defined as a factor. Please make sure that it is a continuous variable."

ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model."
