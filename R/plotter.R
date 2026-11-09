Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      semPathsOptions=NULL,
      initialize=function(options,datamatic,operator,resultsplots) {
            super$initialize(options=options,vars=unlist(c(options$mediators,options$dep,options$factors,options$covs)))
            private$.plotgroup<-resultsplots
            private$.operator<-operator
            private$.datamatic<-datamatic
      },

      initPlots=function() {
           if (!self$options$diagram)
               return()
            private$.plotgroup$diagrams$addItem("0")
            private$.plotgroup$diagrams$get(key = "0")$setTitle("")
      },
      preparePlots=function(image, ggtheme, theme, ...) {
        # The conceptual model is independent of the fitted semPlot diagram.
        # Prepare it before the path-diagram guard, including model-only requests.
        if (self$options$meddiagram)
          private$.prepareModelDiagram()

        if (!self$options$diagram)
          return()
        
        model<-private$.operator$model
        ### this is due to the fact that semPaths seems to fail when an inequality constraints is in ###
        ### check for more lavaanian solution ###
        check<-grep(">|<",model@ParTable$op,invert = T)
        par<-model@ParTable
        model@ParTable<-sapply(par, function(x) x[check],simplify = F)
        ### end ###
      
        
        ## prepare the semPlotModel object to be passed to .rendefun where
        ## semPaths will do the actual diagrams
        
        ## At the moment (mid-2021) semPaths invokes the graphical device even when DoNotPlot is TRUE,
        ## which causes a window to pop up in windows and mac when run in jamovi. So, we need to prepare a semPlot object 
        ## here and make the actual plot in the .renderfun of pathj.b.R with semPaths. 
        ## for multigroup, we trick the .renderfun semPaths() to believe that there's always only one group
        ## per call, because the .renderfun is called for each group.
        
        
        sm<-semPlot::semPlotModel(model)
        groups<-unique(sm@Pars$group)
        images<-private$.plotgroup$diagrams
        
        
        if (all(groups=="")) {
          image<-images$get(key = "0")
          image$setState(list(semModel = sm))
          } else {
          for (i in seq_along(images$itemKeys)) {
            image<-images$get(key = images$itemKeys[[i]])
            .sm<-sm
            .sm@Pars<-.sm@Pars[.sm@Pars$group==groups[i],]
             image$setState(list(semModel = .sm))
          }

          }

        ### images are set ####
        
        ## now we prepare the options for semPaths

        labs<-self$options$diag_paths
        nodeLabels<-model@pta$vnames$ov.num[[1]]
        nodeLabels<-fromb64(nodeLabels)
        if (self$options$diag_abbrev!="0")
            nodeLabels<-abbreviate(nodeLabels,minlength = as.numeric(self$options$diag_abbrev),strict = T)
        
        size<-12
        if (self$options$diag_labsize=="small") size<-8
        if (self$options$diag_labsize=="large") size<-18
        if (self$options$diag_labsize=="vlarge") size<-24
        
        nNodes<-length(nodeLabels)
        size<-size*exp(-nNodes/80)+1
        
        layout<-self$options$diag_type
        rotation<-as.numeric(self$options$diag_rotate)
        if (layout %in% c("circle") & rotation %in% c(2,4)) {
            rotation<-1
            self$warnings<-list(topic="diagram",message=PLOT_WARNS[["rotation"]])
        }
        
        self$semPathsOptions<-list(
                      layout = private$.layout(sm),
                      residuals = self$options$diag_resid,
                      rotation = rotation ,
                      intercepts = F
                      ,nodeLabels=nodeLabels
                      ,whatLabels=labs
                      ,sizeMan = size
                      ,sizeMan2=size/2
                      , curve=2
                      , shapeMan=self$options$diag_shape
                      ,edge.label.cex =1.3)
        

        
        return()
      }
      
      
  ), # end of public
  private = list(
    .diagrams=NULL,
    .datamatic=FALSE,
    .plotgroup=NULL,
    .operator=NULL,
    .layout=function(semModel) {
      if (self$options$diag_type != "mediation")
        return(self$options$diag_type)

      # Pathj uses indirect-effect chains to place nodes by mediation stage.
      # jAMM instead exposes fitted regressions, including expanded contrasts
      # and interactions, so derive the same stages from their directed paths.
      variables <- semModel@Vars$name
      parameters <- as.data.frame(private$.operator$model@ParTable,
                                  stringsAsFactors = FALSE)
      regressions <- parameters[parameters$op == "~" &
        parameters$lhs %in% variables & parameters$rhs %in% variables,
        c("lhs", "rhs")]
      stages <- stats::setNames(rep(1, length(variables)), variables)
      remaining <- variables
      while (length(remaining) > 0) {
        ready <- remaining[vapply(remaining, function(variable) {
          !any(regressions$rhs[regressions$lhs == variable] %in% remaining)
        }, logical(1))]
        # A feedback loop has no left-to-right mediation ordering.
        if (length(ready) == 0)
          return("tree")
        for (variable in ready) {
          parents <- regressions$rhs[regressions$lhs == variable]
          if (length(parents) > 0)
            stages[[variable]] <- max(stages[parents]) + 1
        }
        remaining <- setdiff(remaining, ready)
      }

      # Match Pathj's triangular single-chain layout and spread parallel nodes
      # vertically; keep rows in semPlot's node order, not the sorted stages.
      vertical <- rep(.8, length(variables))
      if (length(unique(stages)) == length(stages)) {
        vertical[stages == min(stages) | stages == max(stages)] <- .2
      } else {
        for (stage in unique(stages)) {
          members <- which(stages == stage)
          vertical[members] <- seq_along(members) / (length(members) + 1)
        }
      }
      cbind(x = unname(stages), y = vertical)
    },
    .prepareModelDiagram=function() {
      # Store only plot data in image state so saved analyses can render again.
      state <- model_diagram_data(self$options)
      private$.plotgroup$pathmodel$setState(list(modelDiagram = state))
      notes <- private$.plotgroup$pathnotes
      notes$deleteRows()
      notes$setVisible(length(state$notes) > 0)
      for (index in seq_along(state$notes))
        notes$addRow(index, list(info = state$notes[[index]]))
    },
    .semPaths=function(options) {
      ### we need this because semPaths does not work with do.call() in windows
        res<-try_hard({
        semPlot::semPaths(object = options$object,
                      layout =options$layout,
                      residuals = options$residuals,
                      rotation = options$rotation,
                      intercepts = options$intercepts,
                      nodeLabels= options$nodeLabels,
                      whatLabels=options$whatLabels,
                      sizeMan = options$sizeMan,
                      sizeMan2=options$sizeMan2,
                      curve=options$curve,
                      shapeMan=options$shapeMan,
                      edge.label.cex =options$edge.label.cex,
                      DoNotPlot=TRUE)

      })
      if (is.something(res$error))
          self$warnings<-list(topic="diagram",message=res$error)
      
      return(res)
    }

  ) # end of private
) # end of class
