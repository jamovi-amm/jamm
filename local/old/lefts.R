print=function() {
  add<-""
  if (self$hasRequired())
    add<-" (R)"
  .cat<-function(what) cat(paste0("\n",what,add,"\n"))
  
  .cat("N variables")
  cat(self$nvars)
  .cat("Independent variables")
  cat(self$independents)
  .cat("Mediators")
  cat(self$mediators)
  .cat("Moderators")
  print(self$moderators)
  .cat("Mediators Models")
  print(self$medmodels)
  .cat("Full Models")
  print(self$fullmodel)
  .cat("Original Mediators Models")
  print(self$medmodels)
  .cat("Original Full Models")
  print(self$fullmodel)
  
  cat("\nPaths\n")
  print(self$M)
  cat("\nIs impossible?\n")
  cat(self$isImpossible)
  cat("\nHas required\n")
  cat(self$hasRequired())
  cat("\nHas suggested\n")
  cat(self$hasSuggested())
  cat("\n")
  if (self$isImpossible) {
    cat("\nImpossible paths\n")
    print(self$impossibles)
  }
  else {
    cat("\n Indirect Effects\n")
    print(self$ieffects)
  }
},



if (diff.index>-1) {
  var predictors = mediatorsTerms[diff.index];
  predictors = removeFromList(mediators,predictors, context);
  if (predictors.length<= moderatorsTerms[diff.index].length) {
    flashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
    var added = diff.changes[diff.index].added;
    for (var i = 0; i < added.length; i++)   
      moderatorsTerms[diff.index]=removeFromList(added[i],moderatorsTerms[diff.index], context);
  } else {
    unflashMGridOptionListControl(ui.moderatorsTerms,ui.modeNote);
  }
}
