ERROR_TABLE="jamovi"

INTERACTION_SYMBOL="__XX__XX__"
FACTOR_SYMBOL="._._._."

ARRAY_DEFAULTS <- c(scaling = "centered", contrasts = "simple")
NO_R_OPTS <- c("mediators","mediatorsTerms","covs","factors","dep","modelTerms")
  


NOTES<-list()

NOTES[["ci"]]<-list("standard"="Standard (Delta method)",
                    "bca"="Bias corrected bootstrap",
                    "perc"="Bootstrap percentiles",
                    "norm"="Parametric bootstrap")


WARNS<-list()
WARNS[["nocenterd"]]<-"Variables {vars} are not centered. Consider using the Covariate Scaling options for easier interpretation of lower-order effects."
WARNS[["noindirect"]]<-"Indirect effects cannot be computed for this model."

DATA_WARNS<-list()
DATA_WARNS[["fac_to_cont"]]<-"Warning: a continuous variable is defined as a factor. Please make sure that it is a continuous variable."
DATA_WARNS[["cont_to_fac"]]<-"Warning: a variable has been converted to a factor."

ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model."


DATA_WARNS<-list()
DATA_WARNS[["fac_to_cont"]]<-"Warning: a continuous variable is defined as a factor. Please make sure that it is a continuous variable."
DATA_WARNS[["cont_to_fac"]]<-"Warning: a variable has been converted to a factor."

PLOT_WARNS<-list()
PLOT_WARNS[["nocircle"]]<-"The diagram cannot be produced. Please try a different layout and rotation combination."
PLOT_WARNS[["rotation"]]<-"{var} layout requires rotation to be `Exogenous top` or `Exogenous bottom`. Rotation has been set to `Exogenous top`."
PLOT_WARNS[["circlelayout"]]<-"Layout has been set to Circle."
PLOT_WARNS[["fail"]]<-"The diagram cannot be displayed. Please try a different layout type."


ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model."


SUB<-list("\u2081","\u2082","\u2083","\u2084","\u2085","\u2086","\u2087","\u2088","\u2089","\u20810",
          "\u20811","\u20812","\u20813","\u20814","\u20815","\u20816","\u20817","\u20818","\u20819","\u20820")

