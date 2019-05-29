ds.annotate.diagram <- function(infos, paths, notes, options, n64) {
  
  if (infos$hasModerators()) {
    notes$addRow("modsin", list(info = "Moderators main effects are not shown"))
    notes$setVisible(TRUE)
  }
  
  if (infos$hasRequired()) {
    notes$addRow("red",
                 list(info = "Red paths indicate required coefficients"))
    notes$setVisible(TRUE)
  }
  if (infos$hasSuggested()) {
    notes$addRow("green",
                 list(info = "Green paths indicate suggested coefficients"))
    notes$setVisible(TRUE)
  }
  if (infos$isImpossible) {
    notes$addRow(
      "purple",
      list(info = "Purple paths indicate coefficients that invalidate the mediation model")
    )
    notes$setVisible(TRUE)
  }
  facnote <- FALSE
  labs <- list()
  for (var64 in infos$independents) {
    var<-jmvcore::fromB64(var64)
    if (var %in% options$factors) {
      facnote <- TRUE
    .note <-
      paste(
        n64$nicecontrasts(var),
        n64$contrastsLabels(var),
        sep = " = ",
        collapse = ", "
      )
    labs[[var]] <-paste("For variable <b>", var, "</b> the contrasts are:", .note)
    }
  }
  if (facnote) {
    note = list(
      "Categorical independent variables (factors) are shown with only one rectangle, but their effect",
      "is estimated using contrast variables"
    )
    for (n in seq_along(note))
      notes$addRow(paste0("squares", n), list(info = note[[n]]))
    for (l in seq_along(labs))
      notes$addRow(paste0("labels", l), list(info = labs[[l]]))
    notes$setVisible(TRUE)

  }
}

ds.initModelInfo <- function(self) {
  

  goon<-TRUE
  infoTable <- self$results$info
  if (!is.something(self$options$dep)) {

    infoTable$addRow(rowKey = "gs1",
                     list(
                       info = "Get started",
                       specs = "",
                       value = "Select the dependent variable"
                     ))
    goon<-FALSE
  }
  if (!is.something(self$options$mediators)) {
    infoTable$addRow(rowKey = "gs2",
                     list(
                       info = "Get started",
                       specs = "",
                       value = "Select at least one mediator"
                     ))
    goon<-FALSE
    
  }
  if (!is.something(self$options$factors) &&
      !is.something(self$options$covs)) {
    infoTable$addRow(
      rowKey = "gs3",
      list(
        info = "Get Started",
        specs = "",
        value = "Select at least one factor or covariate as independent variable"
      )
    )
    goon<-FALSE
    
  } else
    if (!is.something(self$options$modelTerms)) {
      infoTable$addRow(rowKey = "gs4",
                       list(
                         info = "Get Started",
                         specs = "",
                         value = "Fill in the model"
                       ))
    }
  return(goon)
}


ds.modelInfo <- function(infos, self, n64) {
    
  infoTable <- self$results$info
  
  if (infos$hasSuggested())
    infoTable$addRow(
      rowKey = "sug",
      list(info = "Overal Model", value = "The model is not a full-fledged mediational model")
    )
  
  if (infos$hasRequired()) {
    infoTable$addRow(rowKey = "req",
                     list(info = "Overal Model", value = "The mediational model is incomplete"))
  }
  
  if (infos$isEstimable()) {
    
    forms<-infos$original_medmodels
    forms[[length(infos$original_medmodels)+1]]<-infos$original_fullmodel
    modelFormulas<-list()
    for (i in seq_along(forms)) {
      modelFormulas[[i]]<-mf.modelFormula(forms[[i]])
      modelFormulas[[i]]<-n64$translate(modelFormulas[[i]])
    }
    n <- length(modelFormulas)
    infoTable$addRow(rowKey = "modelslabel", list(
      info = "Mediators Models",
      specs = "",
      value = ""
    ))
    for (i in seq_along(modelFormulas[-n])) {
      infoTable$addRow(
        rowKey = paste0("m", i),
        values = list(
          info = "",
          specs = paste0("m", i),
          value = modelFormulas[[i]]
        )
      )
    }
    infoTable$addRow(rowKey = "fulllabel", list(
      info = "Full Model",
      specs = "",
      value = ""
    ))
    infoTable$addRow(
      rowKey = "fullspecs",
      values = list(
        info = "",
        specs = paste0("m", n),
        value = modelFormulas[[n]]
      )
    )
    chains <- infos$ieffects
    nc <- 0
    infoTable$addRow(rowKey = "ielabel", list(
      info = "Indirect Effects",
      specs = "",
      value = ""
    ))
    for (chain in chains) {
      nc <- nc + 1
      specs <- paste0("IE ", nc)
      infoTable$addRow(rowKey = specs, list(
        info = "",
        specs = specs,
        value = .nicifychain64(chain,n64)
      ))
    }
  } else {
    infoTable$addRow(rowKey = "wrong",
                     list(
                       info = "Overall model",
                       specs = "",
                       value = "Model is impossible"
                     ))
    infoTable$addRow(rowKey = "imps",
                     list(
                       info = "Mutually impossible",
                       specs = "",
                       value = ""
                     ))
    
    for (i in seq_along(infos$impossibles))
      infoTable$addRow(rowKey = as.character(i),
                       list(
                         info = "",
                         specs = paste("path", i),
                         value = .nicifychain(jmvcore::fromB64(infos$impossibles[[i]]))
                       ))
  }
}
