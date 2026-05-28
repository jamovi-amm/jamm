j_DEBUG=T
j_INFO=T

ERROR_TABLE="issues"

INTERACTION_SYMBOL="__XX__XX__"
FACTOR_SYMBOL="._._._."

NOTES<-list()

NOTES[["ci"]]<-list("standard"="Standard (Delta method)",
                    "bca"="Bias corrected bootstrap",
                    "perc"="Bootstrap percentiles",
                    "norm"="Parametric bootstrap")


WARNS<-list()

DATA_WARNS<-list()
DATA_WARNS[["fac_to_cont"]]<-"Warming: continuous variable are defined as factor. Please make sure that each is a continuous variable."
DATA_WARNS[["cont_to_fac"]]<-"Warning: variable coerced to factor"

ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model"


SUB<-list("\u2081","\u2082","\u2083","\u2084","\u2085","\u2086","\u2087","\u2088","\u2089","\u20810",
          "\u20811","\u20812","\u20813","\u20814","\u20815","\u20816","\u20817","\u20818","\u20819","\u20820")

